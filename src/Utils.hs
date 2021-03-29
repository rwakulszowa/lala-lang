module Utils where

import qualified Data.List       as List
import qualified Data.Map.Strict as Map
import qualified Typiara.Utils   as TU

import           Data.Map        (Map)

replaceValues :: (Traversable t, Ord a) => [b] -> t a -> (Map a b, t b)
replaceValues = TU.refresh

-- Union multiple maps, as long as no keys overlap.
unionMapsRejectOverlap :: (Ord k) => [Map k v] -> Either k (Map k v)
unionMapsRejectOverlap =
  sequence . unionsWithKey rejectOverlapping . fmap (fmap Right)
  where
    rejectOverlapping k _ _ = Left k
    unionsWithKey f = foldl (Map.unionWithKey f) Map.empty

-- Build a map from a list of (k, v) pairs.
-- Fails if keys overlap.
fromListRejectOverlap :: (Ord k) => [(k, v)] -> Either k (Map k v)
fromListRejectOverlap =
  sequence . Map.fromListWithKey rejectOverlapping . fmap (mapSnd Right)
  where
    rejectOverlapping k _ _ = Left k

-- TODO: just use lenses
mapFst f (a, b) = (f a, b)

mapSnd f (a, b) = (a, f b)

-- TODO: use Bifunctor
mapLeft :: (a -> a') -> Either a b -> Either a' b
mapLeft f (Left x)  = Left $ f x
mapLeft _ (Right x) = Right x

-- Turns a list of `Eq` items into a map of unique `Int` keys, each mapping to one of the provided values.
-- Useful when one needs an `Ord` instance for a non-Ord type.
-- O(n^2).
intoUniqueIdMap :: (Eq a) => [a] -> Map Int a
intoUniqueIdMap as = Map.fromList ([0 ..] `zip` List.nub as)

maybeError err = maybe (Left err) Right
