module DynamicExpression
  ( DynamicExpression(..)
  , intoImplExpr
  ) where

import Data.BinaryTree (BinaryTree(..))
import Expression
  ( Expression(..)
  , ImplExprLeaf(..)
  , ImplementationExpression(..)
  , Literal(..)
  , Ref(..)
  )

-- | An expression usable in the dynamic phase of the program.
-- Represents function application of references and literals.
--
-- Only external references are allowed, hence `String` instead of `Ref`.
type DynamicExpression = BinaryTree (Either Literal String)

intoImplExpr :: DynamicExpression -> ImplementationExpression
intoImplExpr = fmap f
  where
    f (Left lit) = ImplExprLit lit
    f (Right ref) = ImplExprRef (ExternalRef ref)
