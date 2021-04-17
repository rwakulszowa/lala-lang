{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable,
  DeriveTraversable #-}

module Data.BinaryTree
  ( BinaryTree(..)
  , fromList
  ) where

import Data.Aeson.TH (defaultOptions, deriveJSON)

data BinaryTree a
  = Node (BinaryTree a) (BinaryTree a)
  | Leaf a
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

$(deriveJSON defaultOptions ''BinaryTree)

fromList :: [BinaryTree a] -> BinaryTree a
fromList = foldl1 Node
