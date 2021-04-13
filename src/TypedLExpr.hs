module TypedLExpr where

import           Data.List.NonEmpty
import           Data.Parse
import           LalaType
import           LExpr
import           ParsedType
import           Text.ParserCombinators.Parsec
import           Value

-- | LExpression with type annotated result.
-- Enables type checking on the expected value - it will fail if type
-- resolution produces an incompatible type.
data TypedLExpr =
  TypedLExpr
    { typ  :: LalaType
    , expr :: LExpr Value
    }
  deriving (Eq, Show)

data ParsedTypedLExpr =
  ParsedTypedLExpr (ParsedType TypeVarId) (LExpr Value)
  deriving (Eq, Show)

instance Parse ParsedTypedLExpr where
  parser = do
    typ <- parser
    whiteSpace
    char ':'
    whiteSpace
    ParsedTypedLExpr typ <$> parser

into :: ParsedTypedLExpr -> Either String TypedLExpr
into (ParsedTypedLExpr t e) = do
  t <- fromParsedType t
  return (TypedLExpr t e)

parseTLE :: String -> Either String TypedLExpr
parseTLE s = parseString s >>= into
