{-# LANGUAGE OverloadedStrings #-}

module Static.Impl
  ( Impl(..)
  , directDeps
  , lang
  , tosrc
  , bind
  , reorderF
  ) where

import           Data.Foldable
import           Data.List
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Maybe
import qualified Data.Set           as S
import qualified Data.Text          as T
import           LExpr
import           Lang
import           Value

data Impl
  = JsLambda [T.Text] T.Text
  | LalaImpl [T.Text] (LExpr Value)
  deriving (Eq, Show, Ord)

directDeps :: Impl -> S.Set T.Text
directDeps (JsLambda _ _)        = mempty
directDeps (LalaImpl args lexpr) = refs lexpr S.\\ S.fromList args

lang :: Impl -> Lang
lang (JsLambda _ _) = Js
lang (LalaImpl _ _) = Lala

tosrc :: Lang -> Impl -> Maybe T.Text
tosrc Js (JsLambda [] impl) = Just impl
tosrc Js (JsLambda args impl) =
  Just (T.intercalate " => " args <> " => " <> impl)
tosrc Js (LalaImpl args impl) = tosrc Js (JsLambda args (unparse JsSyntax impl))
tosrc Lala (LalaImpl args impl) =
  Just (T.unwords args <> " -> " <> unparse LExprSyntax impl)
tosrc _ _ = Nothing

bind :: Lang -> T.Text -> T.Text -> Maybe T.Text
bind Js id val = Just ("const " <> id <> " = " <> val)
bind _ _ _     = Nothing

-- | Build an implementation that, when applied to a function,
-- returns the same function with arguments reordered.
-- Nothing if order is not a permutation of [0 .. n].
-- TODO: a type safe variant of permuations of [0 .. n].
reorderF :: [Int] -> Maybe Impl
reorderF order =
  if sort order /= naturalOrder
    then Nothing
    else Just
           (LalaImpl
              ("fun" : (mkid <$> naturalOrder))
              (LExpr (LExprLeaf . Ref <$> "fun" :| (mkid <$> order))))
  where
    naturalOrder = [0 .. length order - 1]
    mkid i = T.pack [toEnum (i + 97)]

--
-- LExpr conversions.
--
data Syntax
  = LExprSyntax
  | JsSyntax
  deriving (Eq, Show, Ord)

syntax Js   = JsSyntax
syntax Lala = LExprSyntax

unparse :: Syntax -> LExpr Value -> T.Text
unparse syntax (LExpr (t :| [])) = unparseNode syntax t
unparse syntax (LExpr tokens) =
  let xs = unparseNode syntax <$> toList tokens
   in case syntax of
        LExprSyntax -> wrapParens (T.unwords xs)
        JsSyntax ->
          let (fun:args) = xs
           in fun <> T.unwords (wrapParens <$> args)
  where
    wrapParens x = "(" <> x <> ")"

-- TODO: reuse Value.unparse
unparseNode s (LExprNodeRec n) = unparse s n
unparseNode s (LExprLeaf v)    = unparseValue v

unparseValue (Ref s)              = s
unparseValue (Lit (IntLiteral i)) = T.pack (show i)
unparseValue (Lit (StrLiteral s)) = "\"" <> s <> "\""
