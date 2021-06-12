{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Type
  ( Type(..)
  ) where

import           Data.Data           (Data, Typeable, toConstr)
import           Data.Hashable
import           GHC.Generics
import           Typiara.Data.Tagged (Tagged (..))
import           Typiara.FT          (FT (..))
import           Typiara.TypDef      (TypDef (..), UnifyError (..),
                                      UnifyResult (..))

-- A simple set of types.
-- Prefixed with "C" to avoid clashes with Haskell.
--
-- TODO: add a CSum type representing an enum. Requires a mapper function, (CSum a b => (a -> c) -> (b -> c) -> s -> c).
-- In the long run, CMaybe may be replaced with a CSum.
-- Delayed for now, as CMaybe is sufficient to provide basic monadic functionality.
--
-- TODO: think about whether there's a need to add dynamic (user-defined) types - they could probably be represented as
-- a tuple with a tag.
data Type a
  = CProd a a
  | CSeq a
  | CMaybe a
  | CUnit
  | CBool
  | CNum
  | CStr
  deriving ( Eq
           , Show
           , Read
           , Ord
           , Functor
           , Foldable
           , Traversable
           , Data
           , Typeable
           , Generic
           )

instance TypDef Type where
  unify (CProd a b) (CProd c d) =
    Right (UnifyResult (CProd a b) [(a, c), (b, d)] [])
  unify (CSeq a) (CSeq b) = Right (UnifyResult (CSeq a) [(a, b)] [])
  unify (CMaybe a) (CMaybe b) = Right (UnifyResult (CMaybe a) [(a, b)] [])
  unify x y =
    if x == y
      then Right (UnifyResult x [] [])
      else Left (ConflictingTypes (tag x) (tag y))

instance Tagged Type where
  tag = show . toConstr
  -- TODO: try to reuse the magic `gunfold` function from `Data.Data`.
  fromTag "CUnit" []     = Just CUnit
  fromTag "CBool" []     = Just CBool
  fromTag "CNum" []      = Just CNum
  fromTag "CStr" []      = Just CStr
  fromTag "CProd" [a, b] = Just (CProd a b)
  fromTag "CSeq" [a]     = Just (CSeq a)
  fromTag "CMaybe" [a]   = Just (CMaybe a)
  fromTag _ _            = Nothing

instance (Hashable a) => Hashable (Type a)
