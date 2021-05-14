{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
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
  , deleteUser
  , newUser
  , nextLocal
  , nextGlobal
  , uuidToText
  , textToUUID
  , textToRoomID
  , roomIDToText
  , unRoomID
  ) where
import           Protolude             hiding (Text, empty, local)
import           Data.Aeson
import qualified Data.Set       as Set
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import           Data.UUID      (UUID)
import qualified Data.UUID      as UUID
import           GHC.Generics   ()

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

newRoom :: RoomID -> RoomName -> RoomState' a
newRoom rid rname = RoomState rid rname Set.empty Q.empty Q.empty

nilRoom :: RoomState' a
nilRoom = newRoom (RoomID UUID.nil) ""

newUser :: UserID -> RoomState' a -> RoomState' a
newUser user roomState@RoomState{users}    = roomState{users = Set.insert user users}

deleteUser :: UserID -> RoomState' a -> RoomState' a
deleteUser user roomState@RoomState{users} = roomState{users = Set.delete user users}

qLocal :: a -> RoomState' a -> RoomState' a
qLocal req = ql (|> req)

qGlobal :: a -> RoomState' a -> RoomState' a
qGlobal req = qg (|> req)

nextLocal :: RoomState' a -> RoomState' a
nextLocal = ql next

nextGlobal :: RoomState' a -> RoomState' a
nextGlobal = qg next

ql :: (Queue a -> Queue a) -> RoomState' a -> RoomState' a
ql queueFunc roomState@RoomState{rlocal} = roomState{rlocal=queueFunc rlocal}

qg :: (Queue a -> Queue a) -> RoomState' a -> RoomState' a
qg queueFunc roomState@RoomState{rglobal} = roomState{rglobal=queueFunc rglobal}

data StackType   = LOCAL | BROAD deriving (Eq, Generic, Ord, Read, Show)

data Payload =
  QUEUE { stack :: StackType                                                        } |
  NEXT { stack :: StackType                                                         } |
  LEAVE                                                                               |
  WORLD { attendees :: [UserID], local :: [UserID], broad :: [UserID], name :: Text } |
  ClientInit { attendee :: UserID, room :: RoomID                                   }
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
