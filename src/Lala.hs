module Lala
  ( process
  , Lang(..)
  ) where

import           Data.Bifunctor (first)
import           Data.Parse
import           LalaType
import           Lang
import           LExpr
import           Static
import           Static.Store
import           Value

-- | Resolve, typecheck and transpile a single expression.
-- NOTE: the implementation depends on the "one type per ref" assumption. If the assumption changes,
-- the implementation will have to change, most likely by making type lookup `Bindings`-aware.
process ::
     Store -> Lang -> LExpr Value -> Either ProcessError (LalaType, String)
process store lang lexpr = do
  typ <- infer
  src <- transpile'
  return (typ, src)
  where
    infer =
      first (InferError . unwrapError) .
      inferLExpr . fmap (`ValueWithTypeLookup` typeLookup) $
      lexpr
      where
        typeLookup = typ <$> store
        unwrapError (ILEApplyError f x) = ILEApplyError f (vlValue <$> x)
        unwrapError (ILEInferError x)   = ILEInferError x
    transpile' = first TranspileError (transpile store lang lexpr)

-- TODO: use the magic `:+:` type operator.
data ProcessError
  = TranspileError TranspileError
  | InferError (InferLExprError Value)
  deriving (Eq, Show)
