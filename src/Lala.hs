module Lala
  ( inferValueLExpr
  ) where

import           Data.Bifunctor (first)
import           LalaType
import           LExpr
import           Refs
import           Value

inferValueLExpr ::
     Refs -> LExpr Value -> Either (InferLExprError Value) LalaType
inferValueLExpr refs =
  first unwrapError . inferLExpr . fmap (`ValueWithTypeLookup` typeLookup)
  where
    typeLookup = typ <$> refs
    unwrapError (ILEApplyError f x) = ILEApplyError f (vlValue <$> x)
    unwrapError (ILEInferError x)   = ILEInferError x
