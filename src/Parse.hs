{-# LANGUAGE DeriveTraversable #-}

module Parse
  ( parseExpression
  , parseDynamicExpression
  , parseType
  , ParsedType(..)
  , TypeVarId(..)
  , TypeTag(..)
  , ParsedConstraint(..)
  , SignatureToken(..)
  ) where

import           Data.BinaryTree                        (BinaryTree (..))
import           Data.Char
import           Data.List                              (elemIndex, intercalate)
import           Data.List.NonEmpty                     (NonEmpty (..))
import           Data.List.Split                        (splitOn)
import           Data.Maybe                             (isJust)
import           DynamicExpression
import           Expression                             (Expression (..),
                                                         ImplExprLeaf (..),
                                                         ImplementationExpression (..),
                                                         Literal (..), Ref (..))
import           Types

import           Control.Monad
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token

-- parser boilerplate
wrapParser parser src =
  case parse (standalone parser) "" src of
    Right expr -> Right expr
    Left err   -> Left $ show err

parseDynamicExpression :: String -> Either String DynamicExpression
parseDynamicExpression = wrapParser dynamicExprP

parseExpression :: String -> Either String Expression
parseExpression = wrapParser expressionP

parseType :: String -> Either String (ParsedType TypeVarId)
parseType = wrapParser parsedTypeP

-- Parsec definitions
langDef :: Token.LanguageDef ()
langDef =
  Token.LanguageDef
    { Token.commentStart = "{-"
    , Token.commentEnd = "-}"
    , Token.commentLine = "--"
    , Token.nestedComments = True
    , Token.identStart = letter <|> char '_'
    , Token.identLetter = alphaNum <|> char '_'
    , Token.opStart = oneOf ""
    , Token.opLetter = oneOf ""
    , Token.reservedOpNames = ["="]
    , Token.reservedNames = []
    , Token.caseSensitive = True
    }

lexer = Token.makeTokenParser langDef

parens = Token.parens lexer

brackets = Token.brackets lexer

whiteSpace = Token.whiteSpace lexer

reservedOp = Token.reservedOp lexer

identifier = Token.identifier lexer

stringLiteral = Token.stringLiteral lexer

float = Token.float lexer

integer = Token.integer lexer

commaSep = Token.commaSep lexer

-- Parsers
-- Parsing functions are suffixed with `P`.
standalone parser = do
  whiteSpace
  result <- parser
  eof
  return result

-- | Generic parser for binary trees.
binaryTreeP :: Parser a -> Parser (BinaryTree a)
binaryTreeP leafP = do
  parens node <|> leaf
  where
    leaf = Leaf <$> leafP
    node = do
      a <- binaryTreeP leafP
      b <- binaryTreeP leafP
      return (Node a b)

-- | DynamicExpression parser.
dynamicExprP :: Parser DynamicExpression
dynamicExprP = binaryTreeP (dynRef <|> dynLit)
  where
    dynRef = Right <$> identifier
    dynLit = Left <$> literalP

-- Expression parser.
literalP :: Parser Literal
literalP = strLiteralP <|> intLiteralP

intLiteralP = do
  sign <- optionMaybe $ char '-'
  num <- integer
  return $ IntLiteral $ num * signMultiplier sign

signMultiplier :: Num n => Maybe Char -> n
signMultiplier (Just m) = -1
signMultiplier Nothing  = 1

strLiteralP = StrLiteral <$> stringLiteral

expressionP :: Parser Expression
expressionP =
  try functionExpressionP <|>
  (ImplementationExpression <$> implementationExpressionP)

functionExpressionP :: Parser Expression
functionExpressionP = do
  args <- many1 identifier
  reservedOp "->"
  FunctionExpression args <$> implementationExpressionP

-- | For simplicity, each application expression has to be wrapped in parens.
-- This approach makes parsing the tree much simpler.
-- Once the project matures, consider supporting implicit parens where possible.
implementationExpressionP :: Parser ImplementationExpression
implementationExpressionP = binaryTreeP (constExpressionP <|> refExpressionP)

constExpressionP :: Parser ImplExprLeaf
constExpressionP = ImplExprLit <$> literalP

-- | Local refs start with a lower letter, external ones with an upper letter.
refExpressionP :: Parser ImplExprLeaf
refExpressionP =
  ImplExprRef <$> do
    ref <- identifier
    return $
      if isLower (head ref)
        then LocalRef ref
        else ExternalRef ref

applicationExpressionP :: Parser ImplementationExpression
applicationExpressionP = do
  fun <- implementationExpressionP
  Node fun <$> implementationExpressionP

-- TypeDef parser.
-- `Show a, Eq b => a -> b -> c`
-- Requires further validation before being converted to a `TypeDef`.
data ParsedType a =
  ParsedType
    { constraints :: [ParsedConstraint a]
    , signature   :: SignatureToken a
    }
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Wrapper for type variables defined by the user.
-- The parser will handle them as regular strings, but they will be further
-- processed down the path.
newtype TypeVarId =
  TypeVarId String
  deriving (Eq, Show, Ord)

-- | Raw representation of a type.
-- Should match a `Typiara.Tagged` format.
newtype TypeTag =
  TypeTag
    { unTypeTag :: String
    }
  deriving (Eq, Show, Ord)

-- | The type equation operating on type variables, without constraints.
-- Nodes represent functions: `a -> b`.
-- Leaves represent non-function types, including type application: `a`, `t a`.
--
-- Note, that the current approach doesn't handle parens in type application -
-- `f (a -> b) (c d)` is not yet allowed.
-- TODO: add a third enum handling type application through parens / brackets.
data SignatureToken a
  = SignatureLeaf (NonEmpty a)
  | SignatureNode (SignatureToken a) (SignatureToken a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- `Show a, Eq b`, etc.
data ParsedConstraint a =
  ParsedConstraint
    { constraintId :: TypeTag
    , typeParam    :: a
    }
  deriving (Eq, Show, Functor, Foldable, Traversable)

parsedTypeP :: Parser (ParsedType TypeVarId)
parsedTypeP = do
  constraints <- parsedConstraintP `sepBy` char ','
  reservedOp "=>"
  ParsedType constraints <$> signatureTokenP

-- | The signature, defined on the right side of `=>`.
signatureTokenP = try signatureNodeP <|> signatureLeafP

-- | Single identifier.
signatureLeafP = do
  (x:xs) <- many1 identifier
  return (SignatureLeaf (TypeVarId <$> (x :| xs)))

-- | Single pair with an arrow: `a -> b`
-- If left side is recursive, it must be wrapped in parens,
-- e.g. `(a -> b) -> c`.
-- Right side parens must be omitted. `a -> (b -> c)` is not allowed.
signatureNodeP = do
  left <- parens signatureTokenP <|> signatureLeafP
  reservedOp "->"
  SignatureNode left <$> signatureTokenP

parsedConstraintP :: Parser (ParsedConstraint TypeVarId)
parsedConstraintP = do
  whiteSpace
  constraintId <- identifier
  typeParam <- identifier
  whiteSpace
  return $ ParsedConstraint (TypeTag constraintId) (TypeVarId typeParam)
