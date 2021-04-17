{-# LANGUAGE TemplateHaskell #-}

module TranspilablePack
  ( TranspilablePack(..)
  , transpilePack
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.List (nub)

import qualified Assembly
import qualified DynamicExpression
import qualified Lang
import qualified ProcessedExpression
import qualified ResolvedExpression

import DynamicExpression (DynamicExpression)
import Expression (Expression(..))
import Impl (Impl(..), Src(..))
import LalaType (LalaType, singleton)
import Lang (Lang)
import PieceOfLogic (PieceOfLogic(..))
import Type (Type(..))
import Typiara.FT (FT(Nil))
import Utils (mapLeft)

-- A composite of inputs that can be parsed and transpiled.
--
-- Upon successful transpilation, each pack exports a single expression.
data TranspilablePack =
  TranspilablePack
    { externalPieces :: [(String, PieceOfLogic)] -- existing implementations or expressions to be used
    , expression :: DynamicExpression -- expression to transpile
    , lang :: Lang -- target `Lang` to transpile to
    , id :: String
    }
  deriving (Show, Eq)

$(deriveJSON defaultOptions ''TranspilablePack)

transpilePack :: TranspilablePack -> Either String [Assembly.Assembly]
transpilePack (TranspilablePack externalPieces expr lang id) = do
  resolvedExpressions <-
    mapLeft show .
    ResolvedExpression.resolve
      (buildLookup $ filterPiecesByLang lang externalPieces) $
    ((flip ProcessedExpression.fromExpression $ LalaType.singleton Nil) .
     ImplementationExpression . DynamicExpression.intoImplExpr $
     expr)
  return
    [ Assembly.build lang (ResolvedExpression.pieces resolvedExpr)
    | resolvedExpr <- resolvedExpressions
    ]
  where
    anyOf x candidates = x `Set.member` Set.fromList candidates
    buildLookup :: [(String, PieceOfLogic)] -> String -> [PieceOfLogic]
    buildLookup assocs k =
      Map.findWithDefault [] k $
      Map.fromListWith mappend [(k, [v]) | (k, v) <- assocs]
    filterPiecesByLang lang = filter (pieceMatchesLang lang . snd)
      where
        pieceMatchesLang lang (ExprPiece _) = True
        pieceMatchesLang lang (ImplPiece impl) = Lang.getLang impl == lang
