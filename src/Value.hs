{-# LANGUAGE ScopedTypeVariables #-}

module Value
  ( Value(..)
  , Literal(..)
  , intLiteral
  , strLiteral
  , ref
  ) where

import           Data.Parse
import           LExpr
import           Text.ParserCombinators.Parsec

-- | Items for value-level L-Expressions.
data Value
  = Ref String
  | Lit Literal
  deriving (Eq, Show, Ord)

-- | A minimal set of inlineable values.
-- The set has to be small, as every variant should be representable in every supported language.
-- TODO: introduce sequence literals: IntSeq, StrSeq, NilSeq
data Literal
  = IntLiteral Integer
  | StrLiteral String
  deriving (Show, Eq, Ord)

-- | Convenience constructors.
intLiteral = singleton . Lit . IntLiteral

strLiteral = singleton . Lit . StrLiteral

ref = singleton . Ref

instance Parse Value where
  parser = valueP

valueP :: Parser Value
valueP = refP <|> litP
  where
    refP = Ref <$> identifier
    litP = Lit <$> parser

instance Parse Literal where
  parser = literalP

literalP :: Parser Literal
literalP = strLiteralP <|> intLiteralP
  where
    strLiteralP = StrLiteral <$> stringLiteral
    intLiteralP = do
      sign <- optionMaybe $ char '-'
      let signMultiplier = maybe 1 (const $ -1) sign
      num <- integer
      return $ IntLiteral (num * signMultiplier)
