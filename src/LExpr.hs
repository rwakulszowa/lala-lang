{-# LANGUAGE DeriveTraversable #-}

module LExpr
  ( LExpr(..)
  , LExprNode(..)
  , singleton
  , (|<)
  , inferLExpr
  , InferLExprError(..)
  ) where

import           Control.Monad
import           Data.Bifunctor                (first)
import           Data.Infer
import           Data.List.NonEmpty
import           Data.Parse
import           LalaType                      (LalaType (..), apply, empty)
import           Text.ParserCombinators.Parsec

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
