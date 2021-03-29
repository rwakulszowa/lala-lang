{-# LANGUAGE DeriveTraversable, DeriveDataTypeable,
  MultiParamTypeClasses, FlexibleInstances #-}

module Type
  ( Type(..)
  ) where

import Data.Data (Data, Typeable, toConstr)
import Typiara.Data.Tagged (Tagged(..))
import Typiara.FT (FT(..))
import Typiara.Typ (Typ(..), UnifyError(..), UnifyResult(..))

-- A simple set of types.
-- Prefixed with "C" to avoid clashes with Haskell.
data Type a
  = CSeq a
  | CBool
  | CNum
  | CStr
  deriving (Eq, Show, Read, Ord, Functor, Foldable, Traversable, Data, Typeable)

instance Typ Type where
  unify (CSeq a) (CSeq b) = Right (UnifyResult (CSeq a) [(a, b)])
  unify x y =
    if x == y
      then Right (UnifyResult x [])
      else Left (ConflictingTypes (tag x) (tag y))

instance (Data a) => Tagged Type a where
  tag = show . toConstr
  -- TODO: try to reuse the magic `gunfold` function from `Data.Data`.
  fromTag "CBool" [] = Just CBool
  fromTag "CNum" [] = Just CNum
  fromTag "CStr" [] = Just CStr
  fromTag "CSeq" [a] = Just (CSeq a)
  fromTag _ _ = Nothing
