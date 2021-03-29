-- | Logic turning lala-lang types into text representing source code in supported languages.
-- TODO: clean up the code, remove redundant exports.
-- TODO: this module should handle generic types only. Turning a specific Impl into a string should be handled under `Impl`.
-- TODO: more systematic approach. There should be more intermediate steps, backed by types,
--   between the argument and the final `String`.
module Transpile
  ( dumpLogic
  , transpileTo
  , transpileExpression
  , expr2Lala
  , bind
  ) where

import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import qualified ProcessedExpression

import           Data.BinaryTree     (BinaryTree (..))
import           Data.List           (intercalate)
import           Expression
import           Impl
import           Lang
import           Parse
import           PieceOfLogic
import           ProcessedExpression (ProcessedExpression)
import           Types

-- FIXME: Either String String
impl2Src :: Lang -> String -> Impl -> String
-- Raw impl found -> copy verbatim
impl2Src lang id impl
  | getLang impl == lang = implToStr id impl

dumpLogic :: Lang -> [(String, PieceOfLogic)] -> String
dumpLogic lang = unlines . map (dumpPiece lang)
  where
    dumpPiece lang (id, ImplPiece impl) = impl2Src lang id impl
    dumpPiece lang (id, ExprPiece processedExpr) =
      transpileExpression lang id processedExpr

-- | To avoid name clashes, some languages may need to prefix local tree
-- identifiers with a unique id.
type ExpressionTreeId = String

-- | All trees seen until the current point.
-- NOTE: the newest (i.e. most deep) element is at the head.
type TreePath = [ExpressionTreeId]

treePathToId = intercalate "_" . reverse

type SourceText = String

-- | Transpile all items of a tree, including the result, and bind them to
-- ids based on `treeId`.
-- `treeId` is also the ident to which the final result is bound.
transpileExpression :: Lang -> ExpressionTreeId -> ProcessedExpression -> String
transpileExpression lang treeId expr =
  unlines . map (transpileAndBind lang) $ [([treeId], expr, mempty)]
  where
    transpileAndBind lang (treePath, processedExpression, localRefs) =
      let decorateExternalRefs =
            fmapExternalRefs
              (\refId ->
                 if refId `Set.member` localRefs
                   then treePathToId (refId : tail treePath)
                   else refId)
          ident = treePathToId treePath
          sourceText =
            transpileTo
              lang
              (decorateExternalRefs
                 (ProcessedExpression.expr processedExpression))
       in bind lang ident sourceText

-- | Transpile an expression into a string, without binding it to an id.
transpileTo :: Lang -> Expression -> String
transpileTo Py = expr2Py
transpileTo Js = expr2Js

expr2Py :: Expression -> String
expr2Py (FunctionExpression args impl) =
  inlineSrcToStr $ PyLambda args (impl2Py impl)
expr2Py (ImplementationExpression impl) =
  inlineSrcToStr $ PyLiteral (impl2Py impl)

expr2Js :: Expression -> String
expr2Js (FunctionExpression args impl) =
  inlineSrcToStr $ JsLambda args (impl2Js impl)
expr2Js (ImplementationExpression impl) =
  inlineSrcToStr $ JsLiteral (impl2Js impl)

mapAllButFirst :: (a -> a) -> [a] -> [a]
mapAllButFirst f (x:xs) = x : map f xs

-- Py
literal2Py :: Literal -> String
literal2Py (StrLiteral x) = show x
literal2Py (IntLiteral x) = show x

impl2Py :: ImplementationExpression -> String
impl2Py (Leaf (ImplExprLit x)) = literal2Py x
impl2Py (Leaf (ImplExprRef (LocalRef x))) = x
impl2Py (Leaf (ImplExprRef (ExternalRef x))) = x
impl2Py (Node fun arg) =
  wrapParens $ impl2Py fun ++ " " ++ wrapParens (impl2Py arg)

-- Js
literal2Js :: Literal -> String
literal2Js (StrLiteral x) = show x
literal2Js (IntLiteral x) = show x

impl2Js :: ImplementationExpression -> String
impl2Js (Leaf (ImplExprLit x)) = literal2Js x
impl2Js (Leaf (ImplExprRef (LocalRef x))) = x
impl2Js (Leaf (ImplExprRef (ExternalRef x))) = x
impl2Js (Node fun arg) =
  wrapParens $ impl2Js fun ++ " " ++ wrapParens (impl2Js arg)

-- Lala
literal2Lala :: Literal -> String
literal2Lala (StrLiteral x) = show x
literal2Lala (IntLiteral x) = show x

impl2Lala :: ImplementationExpression -> String
impl2Lala (Leaf (ImplExprLit x)) = literal2Lala x
impl2Lala (Leaf (ImplExprRef (LocalRef x))) = x
impl2Lala (Leaf (ImplExprRef (ExternalRef x))) = x
impl2Lala (Node fun arg)
  -- No need to wrap the argument. In Js and Py, parens are necessary to invoke a function.
  -- In Lala, a whitespace is sufficient.
 = wrapParens $ impl2Lala fun ++ " " ++ impl2Lala arg

-- | Turn an expression into a valid Lala string.
-- The resulting string object is wrapped with parens.
expr2Lala :: Expression -> String
expr2Lala expr =
  case expr of
    (FunctionExpression args impl) -> unwords args ++ " -> " ++ handleImpl impl
    (ImplementationExpression impl) -> handleImpl impl
  where
    handleImpl = impl2Lala

bind :: Lang -> String -> String -> String
bind Js ident impl = "const " ++ ident ++ " = " ++ impl
bind _ ident impl  = ident ++ " = " ++ impl

wrap :: String -> String -> String -> String
wrap open close base = open ++ base ++ close

wrapParens = wrap "(" ")"
