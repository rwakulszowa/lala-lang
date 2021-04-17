module Data.BiDiEqMap
  ( BiDiEqMap
  , fromList
  , lookupByKey
  , lookupByValue
  ) where

import Data.Array (Array, (!), assocs, bounds, inRange, listArray)
import Data.List (find, nub)

-- A Map-like structure for non-Ord keys, allowing lookup in both directions.
-- Less performant than a proper map, of course.
newtype BiDiEqMap a =
  BiDiEqMap (Array Int a)
  deriving (Eq, Show)

fromList xs =
  let uniqueXs = nub xs
   in BiDiEqMap $ listArray (0, length uniqueXs - 1) uniqueXs

lookupByKey :: Int -> BiDiEqMap a -> Maybe a
lookupByKey k (BiDiEqMap arr) =
  if bounds arr `inRange` k
    then Just $ arr ! k
    else Nothing

lookupByValue :: Eq a => a -> BiDiEqMap a -> Maybe Int
lookupByValue v (BiDiEqMap arr) =
  fst <$> find (\(k, val) -> val == v) (assocs arr)
