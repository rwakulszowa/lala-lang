{-# LANGUAGE ScopedTypeVariables #-}

module Expression
  ( Expression(..)
  , ImplementationExpression(..)
  , ImplExprLeaf(..)
  , Literal(..)
  , Ref(..)
  , digExternalRefs
  , applicationExpression
  , intLiteral
  , strLiteral
  , localRef
  , externalRef
  , fmapExternalRefs
  ) where

import           Data.BinaryTree (BinaryTree (..))
import qualified Data.Set        as Set

type ArgId = String

-- | Representation of a single node. A program is made of a tree of `Expression`s.
-- `Expression` is a direct result of parsing a string.
data Expression
  -- | An expression that introduces new bindings (args) on top of an implementation.
  -- In other words - a function.
  -- Only top level expressions are allowed to define functions. Fancy constructs, such as
  -- closures, can be expressed by partially applied functions with explicitly passed arguments.
  = FunctionExpression
      { args :: [ArgId]
      , impl :: ImplementationExpression
      }
  -- | An expression that is either a leaf, or an implementation that reduces more complex
  -- expressions.
  -- Apart from referencing other expressions, it can combine them by function application.
  | ImplementationExpression
      { implExpr :: ImplementationExpression
      }
  deriving (Show, Eq)

-- | Leaf value of `ImplementationExpression`.
data ImplExprLeaf
  = ImplExprRef Ref
  | ImplExprLit Literal
  deriving (Eq, Show, Ord)

-- | An implementation is a simple tree of applications. No other operation is
-- allowed.
type ImplementationExpression = BinaryTree ImplExprLeaf

-- | Internal and external references should never shadow each other.
-- Defining them as separate types should guarantee that.
data Ref
  -- | FunctionExpression arguments.
  = LocalRef String
  -- | External pieces.
  | ExternalRef String
  deriving (Eq, Show, Ord)

-- | Special constructs, such as `[]`, are not allowed. They can be built from regular
-- functions.
-- TODO: consider removing literals and always injecting them as refs.
data Literal
  = IntLiteral Integer
  | StrLiteral String
  deriving (Show, Eq, Ord)

-- | Convenience constructors.
intLiteral = Leaf . ImplExprLit . IntLiteral

strLiteral = Leaf . ImplExprLit . StrLiteral

localRef = Leaf . ImplExprRef . LocalRef

externalRef = Leaf . ImplExprRef . ExternalRef

digExternalRefs :: Expression -> [String]
digExternalRefs expr =
  case expr of
    (FunctionExpression _ impl)     -> go impl
    (ImplementationExpression impl) -> go impl
  where
    go (Leaf (ImplExprRef (ExternalRef ref))) = [ref]
    go (Leaf (ImplExprRef (LocalRef _)))      = []
    go (Leaf (ImplExprLit _))                 = []
    go (Node fun arg)                         = concatMap go [fun, arg]

-- | Apply `args` to `fun`, producing a composite `ImplementationExpression`.
-- `FunctionExpression`s are not allowed, for simplicity. Functions should be injected as `ImplExprRef`s.
applicationExpression ::
     ImplementationExpression
  -> [ImplementationExpression]
  -> ImplementationExpression
applicationExpression = foldl Node

-- | fmap `ExternalRef`s.
-- `ExternalRef`s are the only bit of an `Expression` that communicates with
-- the outside world.
-- This function allows the outside world to adapt an expression if the context
-- in which it exists changes.
--
-- TLDR: if external refs change their ids for whatever reason, so should the
-- expression.
fmapExternalRefs :: (String -> String) -> Expression -> Expression
fmapExternalRefs f expr =
  case expr of
    (FunctionExpression args impl) ->
      FunctionExpression args (goImplExpr f impl)
    (ImplementationExpression impl) ->
      ImplementationExpression (goImplExpr f impl)
  where
    goImplExpr f (Node fun arg) = Node (goImplExpr f fun) (goImplExpr f arg)
    goImplExpr f (Leaf (ImplExprRef (ExternalRef ref))) =
      Leaf (ImplExprRef (ExternalRef (f ref)))
    goImplExpr _ implExpr = implExpr
