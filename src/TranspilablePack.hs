module TranspilablePack
  ( TranspilablePack(..)
  , transpilePack
  ) where

import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set

import           Data.List           (nub)

import qualified Assembly
import qualified DynamicExpression
import qualified Lang
import qualified ProcessedExpression
import qualified ResolvedExpression

import           DynamicExpression   (DynamicExpression)
import           Expression          (Expression (..))
import           Impl                (Impl (..), Src (..))
import           LalaType            (LalaType, singleton)
import           Lang                (Lang)
import           PieceOfLogic        (PieceOfLogic (..))
import           Type                (Type (..))
import           Typiara.FT          (FT (Nil))
import           Utils               (mapLeft)

-- | A composite of inputs that can be parsed and transpiled.
--
-- Upon successful transpilation, each pack exports a single expression.
data TranspilablePack =
  TranspilablePack
    { expression :: DynamicExpression -- expression to transpile
    , lang       :: Lang -- target `Lang` to transpile to
    , id         :: String
    }
  deriving (Show, Eq)

-- | Transpile an item in a given context.
transpilePack ::
     (String -> [PieceOfLogic])
  -> TranspilablePack
  -> Either String [Assembly.Assembly]
transpilePack pieceLookup (TranspilablePack expr lang id) = do
  resolvedExpressions <-
    mapLeft show .
    ResolvedExpression.resolve (filterPiecesByLang lang . pieceLookup) $
    (flip ProcessedExpression.fromExpression (LalaType.singleton Nil) .
     ImplementationExpression . DynamicExpression.intoImplExpr $
     expr)
  return
    [ Assembly.build lang (ResolvedExpression.pieces resolvedExpr)
    | resolvedExpr <- resolvedExpressions
    ]
  where
    filterPiecesByLang lang = filter f
      where
        f (ExprPiece _)    = True
        f (ImplPiece impl) = Lang.getLang impl == lang
