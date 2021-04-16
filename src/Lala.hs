module Lala
  ( inferValueLExpr
  ) where

import           Data.Bifunctor (first)
import           LalaType
import           LExpr
import           Static.Store
import           Value

inferValueLExpr ::
     Store -> LExpr Value -> Either (InferLExprError Value) LalaType
inferValueLExpr refs =
  first unwrapError . inferLExpr . fmap (`ValueWithTypeLookup` typeLookup)
  where
    typeLookup = typ <$> refs
    unwrapError (ILEApplyError f x) = ILEApplyError f (vlValue <$> x)
    unwrapError (ILEInferError x)   = ILEInferError x
