{-# LANGUAGE DeriveTraversable #-}

module LExpr
  ( LExpr(..)
  , LExprNode(..)
  ) where

import           Data.Parse
import           Text.ParserCombinators.Parsec

-- | Expressions are defined in the form of L-Expressions - sequences of function applications.
-- Generic, to allow the same mechanisms to operate on type level and value level.
data LExpr a =
  LExpr
    { fun  :: String
    , args :: [LExprNode a]
    }
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

data LExprNode a
  = LExprNodeRec (LExpr a)
  | LExprLeaf a
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

instance (Parse a) => Parse (LExpr a) where
  parser = lExprP

lExprP :: Parse a => Parser (LExpr a)
lExprP = do
  fun <- identifier
  args <- many lExprNodeP
  return $ LExpr fun args

lExprNodeP :: Parse a => Parser (LExprNode a)
lExprNodeP = parens node <|> leaf
  where
    leaf = LExprLeaf <$> parser
    node = LExprNodeRec <$> lExprP
