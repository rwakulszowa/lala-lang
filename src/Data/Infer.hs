module Data.Infer
  ( Infer(..)
  ) where

import           Data.Bifunctor (first)
import           LalaType

-- | Inferable types.
class Infer a where
  infer :: a -> Either String LalaType

typeCheck :: (Infer a) => LalaType -> a -> Either TypeCheckError ()
typeCheck typ i = do
  inferred <- first TCInferError (infer i)
  merged <- first TCMergeError (merge typ inferred)
  return ()

data TypeCheckError
  = TCInferError String
  | TCMergeError MergeError
  deriving (Eq, Show)

-- To avoid a cyclic dependency, the instance for LalaType is defined here.
instance Infer LalaType where
  infer = Right
