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
  ) where
import           Protolude             hiding (Text, empty, local)
import           Data.Text.Lazy (Text)
import           Data.UUID      (UUID)

import           Data.Queue as Q

newtype RoomID   = RoomID { unRoomID :: UUID } deriving (Eq, Ord, Read, Show)
newtype UserID   = UserID { unUserID :: Text } deriving (Eq, Ord, Read, Show)

type SpeakReq = UserID

data RoomState' a = RoomState {
    roomID :: RoomID,
    global :: Queue a,
    local  :: Queue a
  } deriving (Eq, Ord, Read, Show)

type RoomState = RoomState' SpeakReq

newRoom :: RoomID -> RoomState' a
newRoom rid = RoomState rid Q.empty Q.empty

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
