-- | Externally defined implementations.
-- This module exposes constructors for supported variants and logic
-- converting them into valid source code.
module Impl
  ( Impl(..)
  , Src(..)
  , StandaloneSrc(..)
  , InlineSrc(..)
  , pyDef
  , pyImport
  , pyLambda
  , pyLiteral
  , jsLambda
  , jsLiteral
  , implToStr
  , inlineSrcToStr
  , toSourceCode
  ) where

import Data.Bifunctor (first)
import Data.List (intercalate)
import Expression
import LalaType (LalaType)
import Lang (Lang(..), WithLang, getLang)
import Parse (parseExpression)
import Type

-- An implementation of a single function.
-- One function may have multiple `Impl`s.
data Impl =
  Impl
    { typ :: LalaType
    , src :: Src
    }
  deriving (Eq, Show)

instance WithLang Impl where
  getLang Impl {src = src} = getLang src

-- |Implementation kinds.
-- |TODO: add another layer of types, group per language
data Src
  = StandaloneSrc StandaloneSrc
  | InlineSrc InlineSrc
  deriving (Eq, Show)

-- |Sources that must be defined as standalone objects, e.g. function definitions.
-- |In most cases, their id is baked in during transpilation.
data StandaloneSrc
  = PyDef [String] [String]
  | PyImport [String] String
  deriving (Eq, Show)

-- |Sources that can be used in anonymous contexts, e.g. as an argument or a return value.
-- |They can be initialized without an explicit id. They can be assigned to one through a binding.
data InlineSrc
  = PyLambda [String] String
  | PyLiteral String
  | JsLambda [String] String
  | JsLiteral String
  deriving (Eq, Show)

instance WithLang Src where
  getLang (InlineSrc s) = getLang s
  getLang (StandaloneSrc s) = getLang s

instance WithLang InlineSrc where
  getLang (PyLambda _ _) = Py
  getLang (PyLiteral _) = Py
  getLang (JsLambda _ _) = Js
  getLang (JsLiteral _) = Js

instance WithLang StandaloneSrc where
  getLang (PyDef _ _) = Py
  getLang (PyImport _ _) = Py

-- | Convenience constructors, hiding details about an item being inline or standalone.
pyDef a = StandaloneSrc . PyDef a

pyImport a = StandaloneSrc . PyImport a

pyLambda a = InlineSrc . PyLambda a

pyLiteral = InlineSrc . PyLiteral

jsLambda a = InlineSrc . JsLambda a

jsLiteral = InlineSrc . JsLiteral

joinOnComma = intercalate ", "

indent = ("    " ++)

implToStr :: String -> Impl -> String
implToStr name (Impl _ (InlineSrc s)) = bind (getLang s) name (inlineSrcToStr s)
implToStr name (Impl _ (StandaloneSrc s)) = standaloneSrcToStr name s

inlineSrcToStr :: InlineSrc -> String
inlineSrcToStr (PyLambda args impl) = toSourceCode handleSingle args impl
  where
    handleSingle arg body = "lambda " ++ arg ++ ": " ++ body
inlineSrcToStr (PyLiteral literal) = literal
inlineSrcToStr (JsLambda args impl) = toSourceCode handleSingle args impl
  where
    handleSingle arg body = arg ++ " => " ++ body
inlineSrcToStr (JsLiteral literal) = literal

type IndentedLine = (Int, String)

indentedLineToString :: IndentedLine -> String
indentedLineToString (depth, text) = concat (replicate depth "    ") ++ text

-- | TODO: incredibly slow. Use Text instead of String, zip with depth stored as number, instead of indenting multiple times.
standaloneSrcToStr :: String -> StandaloneSrc -> String
standaloneSrcToStr name (PyDef args implLines) =
  unlines $ map indentedLineToString $ go name args (zip (repeat 0) implLines)
  where
    go :: String -> [String] -> [IndentedLine] -> [IndentedLine]
    go name args impl =
      let topLevelHeader = header name "arg"
          -- ^ header of the exported function
          wrapperHeader = header "wrapper" ""
          -- ^ header of a wrapper, needed to apply an argument to the body on the first call. Arity of 0.
          body = toSourceCode handleSingle args impl
          -- ^ the actual body: curried, unapplied
       in [noIndent topLevelHeader] ++
          (indentBlock $
           [noIndent wrapperHeader] ++
           (indentBlock body) ++ [noIndent "return wrapper () (arg)"])
      where
        handleSingle :: String -> [IndentedLine] -> [IndentedLine]
        handleSingle arg body =
          [noIndent $ header "inner" arg] ++
          (indentBlock body) ++ [noIndent "return inner"]
        header :: String -> String -> String
        header funName argId = "def " ++ funName ++ "(" ++ argId ++ "):"
        indentBlock :: [IndentedLine] -> [IndentedLine]
        indentBlock = map (first (+ 1))
        noIndent :: String -> IndentedLine
        noIndent text = (0, text)
standaloneSrcToStr name (PyImport path id) =
  "from " ++ intercalate "." path ++ " import " ++ id ++ " as " ++ name

bind :: Lang -> String -> String -> String
bind Js ident impl = "const " ++ ident ++ " = " ++ impl
bind _ ident impl = ident ++ " = " ++ impl

--
-- | Type aliases for `toSourceCode`.
--
type InputArg = String

-- | Handle a function of a single argument.
type HandleSingle ret = InputArg -> ret -> ret

-- | Turn a function like structure into curried source code.
-- Each argument adds one closure with a single argument.
-- Tokens are applied one by one.
--
-- Arguments are not validated in any way. Any string will be blindly used to build the result.
--
-- Works for languages using `()` parens as a call operator.
--
-- NOTE: a decision to always curry has been taken for simplicity. It may change in the future, if performance
-- becomes an issue.
toSourceCode :: HandleSingle ret -> [InputArg] -> ret -> ret
toSourceCode handleSingle args body = foldr handleSingle body args
