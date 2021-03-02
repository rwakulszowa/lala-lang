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
  unify Nil a = Right (UnifyResult a [])
  unify a Nil = unify Nil a
  unify (T (CSeq a)) (T (CSeq b)) = Right (UnifyResult (T (CSeq a)) [(a, b)])
  unify (F a b) (F a' b')
    | a == b || a' == b' =
      Right (UnifyResult (F a a) [(a, b), (a, a'), (a, b')])
  -- ^ There is a link on either side. All variables are unified to the same ident, the result is linked.
  unify (F a b) (F a' b') = Right (UnifyResult (F a b) [(a, a'), (b, b')])
  -- ^ No links. Propagate pairwise, but do not introduce any links.
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
