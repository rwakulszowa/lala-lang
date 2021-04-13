{-# LANGUAGE ScopedTypeVariables #-}

module Value
  ( Value(..)
  , Literal(..)
  , intLiteral
  , strLiteral
  , ref
  ) where

import           Data.Infer
import           Data.Map.Strict               (Map, (!?))
import           Data.Parse
import           LalaType                      (LalaType, singletonT)
import           LExpr
import           Text.ParserCombinators.Parsec
import           Type

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

--
-- Type inference.
--
instance Infer Literal where
  infer (IntLiteral _) = Right (singletonT CNum)
  infer (StrLiteral _) = Right (singletonT CStr)

data ValueWithTypeLookup =
  ValueWithTypeLookup Value (Map String LalaType)
  deriving (Eq, Show, Ord)

instance Infer ValueWithTypeLookup where
  infer (ValueWithTypeLookup (Lit l) _) = infer l
  infer v@(ValueWithTypeLookup (Ref r) m) =
    case m !? r of
      Nothing  -> Left ("Type for " ++ r ++ " not found")
      (Just t) -> Right t
