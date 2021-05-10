module Data.Types
  (
    RoomID(..)
  , UserID(..)
  , RoomState(..)
  , newRoom
  ) where
import           Protolude             hiding (Text)
import           Data.Text.Lazy (Text)
import           Data.UUID      (UUID)

import           Data.Queue as Q

newtype RoomID   = RoomID { unRoomID   :: UUID } deriving (Eq, Ord, Read, Show)
newtype UserID   = UserID { unUserID   :: Text } deriving (Eq, Ord, Read, Show)


data RoomState = RoomState {
    roomID :: RoomID,
    global :: Queue UserID,
    local  :: Queue UserID
  } deriving (Eq, Ord, Read, Show)

newRoom :: RoomID -> RoomState
newRoom rid = RoomState rid Q.empty Q.empty
