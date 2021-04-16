module Data.OrderedMap
  ( OrderedMap(..)
  , ominsert
  , ominsertOrDrop
  , omnull
  , fromList
  , omlookup
  , ommember
  , omassocs
  , leftBiasedConcat
  ) where

import qualified Data.Map   as M
import           Data.Maybe (isJust)

-- | A map that remembers the order in which keys were inserted.
data OrderedMap k v =
  OrderedMap
    { omdata   :: M.Map k v
    -- Inverse order. The oldest element is at the end.
    , revOrder :: [k]
    }
  deriving (Eq, Show, Ord)

-- | Insert a new element.
-- `Nothing` if key already exists.
ominsert :: (Ord k) => k -> v -> OrderedMap k v -> Maybe (OrderedMap k v)
ominsert k v om
  | k `M.member` omdata om = Nothing
ominsert k v (OrderedMap omdata revOrder) =
  Just (OrderedMap (M.insert k v omdata) (k : revOrder))

-- | Insert an item if it hasn't been seen yet.
-- Drop the item otherwise, return `om` untouched.
ominsertOrDrop :: (Ord k) => k -> v -> OrderedMap k v -> OrderedMap k v
ominsertOrDrop k v om =
  case ominsert k v om of
    (Just om') -> om'
    Nothing    -> om

omnull :: (Ord k) => OrderedMap k v
omnull = OrderedMap mempty mempty

fromList :: (Ord k) => [(k, v)] -> OrderedMap k v
fromList = foldl (flip $ uncurry ominsertOrDrop) omnull

omlookup :: (Ord k) => OrderedMap k v -> k -> Maybe v
omlookup om = (`M.lookup` omdata om)

ommember :: (Ord k) => OrderedMap k v -> k -> Bool
ommember om = isJust . omlookup om

-- | (k, v) pairs, sorted by (inverted) insertion order.
omassocs :: (Ord k) => OrderedMap k v -> [(k, v)]
omassocs (OrderedMap d ord) = [(k, d M.! k) | k <- ord]

leftBiasedConcat :: (Ord k) => [OrderedMap k v] -> OrderedMap k v
leftBiasedConcat = foldl concatPair omnull
  where
    concatPair x y = foldr (uncurry ominsertOrDrop) x (omassocs y)
