{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Value
  ( Value(..)
  , Literal(..)
  , ValueWithTypeLookup(..)
  ) where

import           Data.Bifunctor                (first)
import           Data.Infer
import           Data.Map.Strict               (Map, (!?))
import           Data.Parse
import qualified Data.Text                     as T
import           LalaType                      (LalaType, singletonT)
import           Text.ParserCombinators.Parsec
import           Type

-- | Items for value-level L-Expressions.
data Value
  = Ref T.Text
  | Lit Literal
  deriving (Eq, Show, Ord)

-- | A minimal set of inlineable values.
-- The set has to be small, as every variant should be representable in every supported language.
-- TODO: introduce sequence literals: IntSeq, StrSeq, NilSeq
data Literal
  = IntLiteral Integer
  | StrLiteral T.Text
  deriving (Show, Eq, Ord)

instance Parse Value where
  parser = valueP

valueP :: Parser Value
valueP = refP <|> litP
  where
    refP = Ref . T.pack <$> identifier
    litP = Lit <$> parser

instance Unparse Value where
  unparse (Ref r) = r
  unparse (Lit l) = unparse l

instance Parse Literal where
  parser = literalP

literalP :: Parser Literal
literalP = strLiteralP <|> intLiteralP
  where
    strLiteralP = StrLiteral . T.pack <$> stringLiteral
    intLiteralP = IntLiteral <$> signedInteger

instance Unparse Literal where
  unparse (StrLiteral s) = "\"" <> s <> "\""
  unparse (IntLiteral i) = T.pack (show i)

--
-- Type inference.
--
instance Infer Literal where
  infer (IntLiteral _) = Right (singletonT CNum)
  infer (StrLiteral _) = Right (singletonT CStr)

data ValueWithTypeLookup =
  ValueWithTypeLookup
    { vlValue  :: Value
    , vlLookup :: Map T.Text LalaType
    }
  deriving (Eq, Show, Ord)

instance Infer ValueWithTypeLookup where
  infer (ValueWithTypeLookup (Lit l) _) = infer l
  infer v@(ValueWithTypeLookup (Ref r) m) =
    case m !? r of
      Nothing  -> Left ("Type for " ++ T.unpack r ++ " not found")
      (Just t) -> Right t
