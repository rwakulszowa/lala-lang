-- | Parsing and unparsing logic.
-- Parsers operate on `String`s, because that seems to be the more approachable Parsec interface.
-- All further processing is done on `Text`.
module Data.Parse
  ( Parse(..)
  , parens
  , parseString
  , brackets
  , commaSep
  , whiteSpace
  , identifier
  , stringLiteral
  , integer
  , float
  , signed
  , reservedOp
  , symbol
  , Unparse(..)
  ) where

import           Data.Functor.Identity
import qualified Data.Text                              as T
import           Text.Parsec
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token

-- | Parseable types.
class Parse a where
  parser :: Parser a

parseString :: (Parse a) => String -> Either String a
parseString s =
  case parse (standalone parser) "" s of
    Right x -> Right x
    Left e  -> Left (show e)

standalone parser = do
  whiteSpace
  result <- parser
  eof
  return result

-- Parsec setup.
langDef :: Token.LanguageDef ()
langDef =
  emptyDef
    {Token.identStart = letter, Token.identLetter = alphaNum <|> char '_'}

lexer = Token.makeTokenParser langDef

parens = Token.parens lexer

brackets = Token.brackets lexer

commaSep = Token.commaSep lexer

whiteSpace = Token.whiteSpace lexer

identifier = Token.identifier lexer

stringLiteral = Token.stringLiteral lexer

integer = Token.integer lexer

float = Token.float lexer

reservedOp = Token.reservedOp lexer

symbol = Token.symbol lexer

signed :: Num n => ParsecT String u Identity n -> ParsecT String u Identity n
signed n = do
  sign <- optionMaybe $ char '-'
  let signMultiplier = maybe 1 (const $ -1) sign
  num <- n
  return $ num * signMultiplier

-- | Un-Parseable types.
-- unparse . parse = id
class (Parse a) =>
      Unparse a
  where
  unparse :: a -> T.Text
