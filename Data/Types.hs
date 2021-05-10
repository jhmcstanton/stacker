{-# LANGUAGE NamedFieldPuns #-}
module Data.Types
  (
    RoomID(..)
  , UserID(..)
  , RoomState'(..)
  , RoomState
  , SpeakReq
  , clear
  , clearGlobal
  , clearLocal
  , newRoom
  , qGlobal
  , qLocal
  , deleteUser
  , newUser
  ) where
import           Protolude             hiding (Text, empty, local)
import           Data.Set       (Set)
import qualified Data.Set       as Set
import           Data.Text.Lazy (Text)
import           Data.UUID      (UUID)

import           Data.Queue as Q

newtype RoomID   = RoomID { unRoomID :: UUID } deriving (Eq, Ord, Read, Show)
newtype UserID   = UserID { unUserID :: Text } deriving (Eq, Ord, Read, Show)

type SpeakReq = UserID
type RoomName = Text

data RoomState' a = RoomState {
    roomID   :: RoomID,
    roomName :: RoomName,
    users    :: Set UserID,
    global   :: Queue a,
    local    :: Queue a
  } deriving (Eq, Ord, Read, Show)

type RoomState = RoomState' SpeakReq

newRoom :: RoomID -> RoomName -> RoomState' a
newRoom rid rname = RoomState rid rname Set.empty Q.empty Q.empty

newUser :: UserID -> RoomState' a -> RoomState' a
newUser user roomState@RoomState{users}    = roomState{users = Set.insert user users}

deleteUser :: UserID -> RoomState' a -> RoomState' a
deleteUser user roomState@RoomState{users} = roomState{users = Set.delete user users}

qLocal :: a -> RoomState' a -> RoomState' a
qLocal req = ql (|> req)

qGlobal :: a -> RoomState' a -> RoomState' a
qGlobal req = qg (|> req)

clearLocal :: RoomState' a -> RoomState' a
clearLocal = ql (const empty)

clearGlobal :: RoomState' a -> RoomState' a
clearGlobal = qg (const empty)

clear :: RoomState' a -> RoomState' a
clear = clearGlobal . clearLocal

ql :: (Queue a -> Queue a) -> RoomState' a -> RoomState' a
ql queueFunc roomState@RoomState{local} = roomState{local=queueFunc local}

qg :: (Queue a -> Queue a) -> RoomState' a -> RoomState' a
qg queueFunc roomState@RoomState{global} = roomState{global=queueFunc global}
