{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module ParsedType
  ( ParsedType(..)
  , TypeVarId(..)
  , TypeTag(..)
  , ParsedConstraint(..)
  , SignatureToken(..)
  , unParseType
  ) where

import           Data.List                     (intercalate)
import           Data.List.NonEmpty            (NonEmpty (..), toList)
import           Data.Parse

import           Text.ParserCombinators.Parsec

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
  TypeVarId
    { unTypeVarId :: String
    }
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

instance Parse (ParsedType TypeVarId) where
  parser = parsedTypeP

parsedTypeP :: Parser (ParsedType TypeVarId)
parsedTypeP = do
  constraints <- parsedConstraintP `sepBy` char ','
  reservedOp "=>"
  ParsedType constraints <$> signatureTokenP

unParseType :: ParsedType TypeVarId -> String
unParseType (ParsedType c s) =
  let constraints = intercalate ", " $ unParseConstraint <$> c
      signature = unParseSignature s
   in constraints ++ " => " ++ signature

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

unParseSignature (SignatureLeaf as) = unwords $ unTypeVarId <$> toList as
unParseSignature (SignatureNode a b) =
  let left =
        case a of
          (SignatureNode _ _) -> "(" ++ unParseSignature a ++ ")"
          _                   -> unParseSignature a
      right = unParseSignature b
   in left ++ " -> " ++ right

parsedConstraintP :: Parser (ParsedConstraint TypeVarId)
parsedConstraintP = do
  whiteSpace
  constraintId <- identifier
  typeParam <- identifier
  whiteSpace
  return $ ParsedConstraint (TypeTag constraintId) (TypeVarId typeParam)

unParseConstraint (ParsedConstraint (TypeTag t) (TypeVarId tv)) = t ++ " " ++ tv
