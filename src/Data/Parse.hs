module Data.Parse
  ( Parse(..)
  , parens
  , brackets
  , whiteSpace
  , identifier
  , stringLiteral
  , integer
  , reservedOp
  ) where

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token

-- | Parseable types.
class Parse a where
  parser :: Parser a

-- Parsec setup.
langDef :: Token.LanguageDef ()
langDef =
  emptyDef {Token.identStart = upper, Token.identLetter = alphaNum <|> char '_'}

lexer = Token.makeTokenParser langDef

parens = Token.parens lexer

brackets = Token.brackets lexer

whiteSpace = Token.whiteSpace lexer

identifier = Token.identifier lexer

stringLiteral = Token.stringLiteral lexer

integer = Token.integer lexer

reservedOp = Token.reservedOp lexer
