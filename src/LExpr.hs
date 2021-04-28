{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module LExpr
  ( LExpr(..)
  , LExprNode(..)
  , singleton
  , (|<)
  , inferLExpr
  , InferLExprError(..)
  , intLiteral
  , strLiteral
  , ref
  , refs
  ) where

import           Control.Monad
import           Data.Bifunctor                (first)
import           Data.Foldable                 (toList)
import           Data.Infer
import           Data.List.NonEmpty            (NonEmpty (..))
import           Data.Maybe
import           Data.Parse
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import           LalaType                      (LalaType (..), apply, empty)
import           Text.ParserCombinators.Parsec
import           Value

-- | Expressions are defined in the form of L-Expressions - sequences of function applications.
-- Generic, to allow the same mechanisms to operate on type level and value level.
newtype LExpr a =
  LExpr (NonEmpty (LExprNode a))
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

singleton :: a -> LExpr a
singleton x = LExpr (LExprLeaf x :| [])

-- | Application of LExprs.
-- If the added node is a singleton, it's appended to the list.
-- Otherwise it's first wrapped in a node before being appended.
(|<) :: LExpr a -> LExpr a -> LExpr a
(|<) (LExpr (f :| args)) (LExpr (x :| [])) = LExpr (f :| (args ++ [x]))
(|<) (LExpr (f :| args)) x = LExpr (f :| (args ++ [LExprNodeRec x]))

data LExprNode a
  = LExprNodeRec
      { unRec :: LExpr a
      }
  | LExprLeaf
      { unLeaf :: a
      }
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

--
-- Parsing.
--
instance (Parse a) => Parse (LExpr a) where
  parser = lExprP

lExprP :: Parse a => Parser (LExpr a)
lExprP = do
  (fun:args) <- many1 lExprNodeP
  return $ LExpr (fun :| args)

lExprNodeP :: Parse a => Parser (LExprNode a)
lExprNodeP = parens node <|> leaf
  where
    leaf = LExprLeaf <$> parser
    node = LExprNodeRec <$> lExprP

instance (Unparse a) => Unparse (LExpr a) where
  unparse (LExpr nodes) = T.unwords (unparseNode <$> toList nodes)
    where
      unparseNode (LExprNodeRec rec) = "(" <> unparse rec <> ")"
      unparseNode (LExprLeaf a)      = unparse a

--
-- Type inference.
--
inferLExpr :: (Infer a) => LExpr a -> Either (InferLExprError a) LalaType
inferLExpr (LExpr (fun :| args)) = infer' fun >>= (\f -> foldM apply' f args)
  where
    apply' ft x = do
      xt <- infer' x
      first (const (ILEApplyError ft x)) (apply ft xt)
    infer' (LExprLeaf x)    = first ILEInferError (infer x)
    infer' (LExprNodeRec x) = inferLExpr x

data InferLExprError a
  = ILEApplyError
      { f :: LalaType
      , x :: LExprNode a
      }
  | ILEInferError String
  deriving (Eq, Show)

--
-- LExpr Value utils.
--
-- | Convenience constructors.
intLiteral = singleton . Lit . IntLiteral

strLiteral = singleton . Lit . StrLiteral

ref = singleton . Ref

-- | Dig references.
refs :: LExpr Value -> S.Set T.Text
refs = S.fromList . mapMaybe refOrNothing . toList
  where
    refOrNothing (Ref s) = Just s
    refOrNothing _       = Nothing
