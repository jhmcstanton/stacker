module Data.Queue
  (
    Queue
  , empty
  ) where
import           Protolude            hiding (empty)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

newtype Queue a = Q (Seq a) deriving (Eq, Ord, Read, Show)

empty :: Queue a
empty = Q Seq.empty
