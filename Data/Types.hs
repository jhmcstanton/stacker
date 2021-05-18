{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Types
  (
    RoomID(..)
  , UserID(..)
  , RoomState'(..)
  , RoomState
  , SpeakReq
  , StackType(..)
  , Payload(..)
  , newRoom
  , nilRoom
  , qGlobal
  , qLocal
  , q
  , deleteUser
  , newUser
  , nextLocal
  , nextGlobal
  , lengthLocal
  , reorder
  , cancelStack
  , peakNextGlobal
  , userList
  , uuidToText
  , textToUUID
  , textToRoomID
  , roomIDToText
  , unRoomID
  ) where
import           Protolude              hiding (Text, empty, local, sort)
import           Data.Aeson
import qualified Data.Set        as Set
import           Data.Text.Lazy  (Text)
import qualified Data.Text.Lazy  as Text
import           Data.UUID       (UUID)
import qualified Data.UUID       as UUID
import           GHC.Generics    ()

import           Data.Queue as Q

newtype RoomID   = RoomID UUID deriving (Eq, Generic, Ord, Read, Show)
newtype UserID   = UserID Text deriving (Eq, Generic, Ord, Read, Show)

unRoomID :: RoomID -> UUID
unRoomID (RoomID u) = u

uuidToText :: UUID -> Text
uuidToText = Text.fromStrict . UUID.toText

textToUUID :: Text -> Maybe UUID
textToUUID = UUID.fromText . Text.toStrict

textToRoomID :: Text -> Maybe RoomID
textToRoomID = fmap RoomID . textToUUID

roomIDToText :: RoomID -> Text
roomIDToText = uuidToText . unRoomID

type SpeakReq = UserID
type RoomName = Text

data RoomState' a = RoomState {
    roomID    :: RoomID,
    roomName  :: RoomName,
    users     :: Set UserID,
    rglobal   :: Queue a,
    rlocal    :: Queue a
  } deriving (Eq, Generic, Ord, Read, Show)

type RoomState = RoomState' SpeakReq

userList :: RoomState' a -> [UserID]
userList = Set.toList . users

newRoom :: RoomID -> RoomName -> RoomState' a
newRoom rid rname = RoomState rid rname Set.empty Q.empty Q.empty

nilRoom :: RoomState' a
nilRoom = newRoom (RoomID UUID.nil) ""

-- Returns Nothing when username is taken
newUser :: UserID -> RoomState' a -> Maybe (RoomState' a)
newUser user roomState@RoomState{users} =
  if Set.member user users
  then Nothing
  else Just roomState{users = Set.insert user users}

deleteUser :: UserID -> RoomState -> RoomState
deleteUser user roomState@RoomState{users, rglobal, rlocal} = roomState{
    users = Set.delete user users,
    rglobal = Q.remove user rglobal,
    rlocal  = Q.remove user rlocal
  }

qLocal :: a -> RoomState' a -> RoomState' a
qLocal req = ql (|> req)

qGlobal :: a -> RoomState' a -> RoomState' a
qGlobal req = qg (|> req)

q :: StackType -> a -> RoomState' a -> RoomState' a
q = stackToFunc qLocal qGlobal

nextLocal :: RoomState' a -> RoomState' a
nextLocal = ql next

nextGlobal :: RoomState' a -> RoomState' a
nextGlobal = qg next

lengthLocal :: RoomState' a -> Int
lengthLocal = Q.depth . rlocal

peakNextGlobal :: RoomState' a -> Maybe a
peakNextGlobal = Q.peak . rglobal

reorder :: forall a. Ord a => [a] -> StackType -> RoomState' a -> RoomState' a
reorder suggestedList = stackToFunc (ql replaceQ) (qg replaceQ) where
  replaceQ :: Queue a -> Queue a
  replaceQ existingQueue =
    -- Checks that the existing and suggested queue at least contain
    -- the same elements
    if Q.sort suggestedQueue == Q.sort existingQueue
    then suggestedQueue
    else existingQueue
  suggestedQueue :: Queue a
  suggestedQueue = Q.fromList suggestedList

cancelStack :: forall a. Ord a => [a] -> StackType -> RoomState' a -> RoomState' a
cancelStack suggestedList = stackToFunc (ql replaceQ) (qg replaceQ) where
  replaceQ :: Queue a -> Queue a
  replaceQ existingQueue =
    -- Checks that the existing and suggested queue at least contain
    -- the same elements
    if Q.depth suggestedQueue == Q.depth existingQueue - 1
    then suggestedQueue
    else existingQueue
  suggestedQueue :: Queue a
  suggestedQueue = Q.fromList suggestedList

ql :: (Queue a -> Queue a) -> RoomState' a -> RoomState' a
ql queueFunc roomState@RoomState{rlocal} = roomState{rlocal=queueFunc rlocal}

qg :: (Queue a -> Queue a) -> RoomState' a -> RoomState' a
qg queueFunc roomState@RoomState{rglobal} = roomState{rglobal=queueFunc rglobal}

stackToFunc :: (a -> b) -> (a -> b) -> StackType -> (a -> b)
stackToFunc local _ LOCAL = local
stackToFunc _ broad BROAD = broad

data StackType = LOCAL | BROAD deriving (Eq, Generic, Ord, Read, Show)

data Payload =
  QUEUE   { stack :: StackType                                                      } |
  QOTHER  { stack :: StackType, other :: UserID                                     } |
  NEXT    { stack :: StackType                                                      } |
  LEAVE                                                                               |
  CLOSE                                                                               |
  REORDER { stack :: StackType, newstack :: [UserID]                                } |
  CANCEL  { stack :: StackType, newstack :: [UserID]                                } |
  WORLD { attendees :: [UserID], local :: [UserID], broad :: [UserID], name :: Text } |
  CLIENTINIT { attendee :: UserID, room :: RoomID                                   } |
  DUPLICATEUSER { attendees :: [UserID]                                             }
  deriving (Eq, Generic, Ord, Read, Show)


instance FromJSON UserID
instance ToJSON   UserID
instance FromJSON RoomID
instance ToJSON   RoomID
instance FromJSON StackType
instance ToJSON   StackType
instance ToJSON   Payload where
  toJSON    = genericToJSON    defaultOptions { sumEncoding = TaggedObject "action" "" }
instance FromJSON Payload where
  parseJSON = genericParseJSON defaultOptions { sumEncoding = TaggedObject "action" "" }
