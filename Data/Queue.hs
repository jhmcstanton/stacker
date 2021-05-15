{-# Language DerivingVia #-}
{-# Language StandaloneDeriving #-}
module Data.Queue
  (
    Queue
  , empty
  , next
  , remove
  , peak
  , depth
  , (|>)
  ) where
import           Protolude            hiding (empty, on)
import qualified Data.Sequence as Seq

newtype Queue a = Q (Seq a) deriving (Eq, Ord, Read, Show)
deriving via Seq instance Foldable Queue

empty :: Queue a
empty = Q Seq.empty

(|>) :: Queue a -> a -> Queue a
q |> x = on (Seq.|> x) q

next :: Queue a -> Queue a
next = on (Seq.drop 1)

remove :: Eq a => a -> Queue a -> Queue a
remove el = on (Seq.filter (/= el))

peak :: Queue a -> Maybe a
peak (Q s) = Seq.lookup 0 s

depth :: Queue a -> Int
depth (Q s) = Seq.length s

on :: (Seq a -> Seq a) -> Queue a -> Queue a
on f (Q s) = Q $ f s
