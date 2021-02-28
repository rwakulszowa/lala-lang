{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Data.BinaryTree
  ( BinaryTree(..)
  , fromList
  ) where

data BinaryTree a
  = Node (BinaryTree a) (BinaryTree a)
  | Leaf a
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

fromList :: [BinaryTree a] -> BinaryTree a
fromList = foldl1 Node
