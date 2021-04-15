module Static.Impl
  ( Impl(..)
  , directDeps
  , lang
  , tosrc
  ) where

import           Data.Foldable
import           Data.List
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Maybe
import qualified Data.Set           as S
import           Lang
import           LExpr
import           Value

data Impl
  = JsLambda [String] String
  | LalaImpl [String] (LExpr Value)
  deriving (Eq, Show, Ord)

directDeps :: Impl -> S.Set String
directDeps (JsLambda _ _) = mempty
directDeps (LalaImpl args lexpr) = refs lexpr S.\\ S.fromList args
  where
    refs = S.fromList . mapMaybe refOrNothing . toList
      where
        refOrNothing (Ref s) = Just s
        refOrNothing _       = Nothing

lang :: Impl -> Lang
lang (JsLambda _ _) = Js
lang (LalaImpl _ _) = Lala

tosrc :: Lang -> Impl -> Maybe String
tosrc Js (JsLambda [] impl) = Just impl
tosrc Js (JsLambda args impl) = Just (intercalate " => " args ++ " => " ++ impl)
tosrc Js (LalaImpl args impl) = tosrc Js (JsLambda args (unparse JsSyntax impl))
tosrc Lala (LalaImpl args impl) =
  Just (unwords args ++ " -> " ++ unparse LExprSyntax impl)
tosrc _ _ = Nothing

--
-- LExpr conversions.
--
data Syntax
  = LExprSyntax
  | JsSyntax
  deriving (Eq, Show, Ord)

unparse :: Syntax -> LExpr Value -> String
unparse syntax (LExpr (t :| [])) = unparseNode syntax t
unparse syntax (LExpr tokens) =
  let xs = unparseNode syntax <$> toList tokens
   in case syntax of
        LExprSyntax -> "(" ++ unwords xs ++ ")"
        JsSyntax ->
          let (fun:args) = xs
           in fun ++ "(" ++ intercalate ", " args ++ ")"

unparseNode s (LExprNodeRec n) = unparse s n
unparseNode s (LExprLeaf v)    = unparseValue v

unparseValue (Ref s)              = s
unparseValue (Lit (IntLiteral i)) = show i
unparseValue (Lit (StrLiteral s)) = "\"" ++ s ++ "\""
