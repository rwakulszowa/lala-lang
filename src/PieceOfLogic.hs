module PieceOfLogic
  ( PieceOfLogic(..)
  , pieceDeps
  , getType
  ) where

import           Data.Set            (Set)
import           Impl                (Impl, typ)
import           LalaType
import           ProcessedExpression (ProcessedExpression, declaredType,
                                      externalRefs)
import           Type

data PieceOfLogic
  = ImplPiece Impl
  | ExprPiece ProcessedExpression
  deriving (Eq, Show)

pieceDeps :: PieceOfLogic -> Set String
pieceDeps (ImplPiece _)        = mempty
pieceDeps (ExprPiece procExpr) = externalRefs procExpr

getType :: PieceOfLogic -> LalaType
getType (ImplPiece impl)     = typ impl
getType (ExprPiece procExpr) = declaredType procExpr
