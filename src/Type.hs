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
data Type a
  = CSeq a
  | CMaybe a
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
  unify (CSeq a) (CSeq b) = Right (UnifyResult (CSeq a) [(a, b)])
  unify (CMaybe a) (CMaybe b) = Right (UnifyResult (CMaybe a) [(a, b)])
  unify x y =
    if x == y
      then Right (UnifyResult x [])
      else Left (ConflictingTypes (tag x) (tag y))

instance Tagged Type where
  tag = show . toConstr
  -- TODO: try to reuse the magic `gunfold` function from `Data.Data`.
  fromTag "CBool" []   = Just CBool
  fromTag "CNum" []    = Just CNum
  fromTag "CStr" []    = Just CStr
  fromTag "CSeq" [a]   = Just (CSeq a)
  fromTag "CMaybe" [a] = Just (CMaybe a)
  fromTag _ _          = Nothing

instance (Hashable a) => Hashable (Type a)
