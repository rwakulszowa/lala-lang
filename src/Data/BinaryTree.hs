{-# LANGUAGE DeriveTraversable #-}

module Data.BinaryTree
  ( BinaryTree(..)
  , fromList
  , preOrder
  , unPreOrder
  ) where

data BinaryTree a
  = Node (BinaryTree a) (BinaryTree a)
  | Leaf a
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

fromList :: [BinaryTree a] -> BinaryTree a
fromList = foldl1 Node

-- | Traverse from left to right, starting at the root.
-- Nodes are represented as `Nothing`, leaves are represented as `Just`.
preOrder :: BinaryTree a -> [Maybe a]
preOrder (Leaf x)   = [Just x]
preOrder (Node a b) = [Nothing] <> preOrder a <> preOrder b

-- | Reverse of `preOrder`.
-- Iterate items until hitting a Just. Upon encountering one, continue in
-- the next branch.
-- TODO: handle invalid inputs gracefully.
unPreOrder :: [Maybe a] -> BinaryTree a
unPreOrder xs =
  let (t, []) = go xs
   in t
  where
    go ((Just x):xs) = (Leaf x, xs)
    go (Nothing:xs) =
      let (left, xs') = go xs
          (right, xs'') = go xs'
       in (Node left right, xs'')
