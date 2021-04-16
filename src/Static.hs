-- | The language is divided into a static and a dynamic part.
-- The static part is responsible for defining context in which dynamic expressions
-- will be resolved.
-- This module exposes a user friendly interface to the core static functionality:
--  - context (aka Store)
--  - dependency resolution
--  - transpilation
module Static
  ( transpile
  , TranspileError(..)
  , Store
  , Lang(..)
  ) where

import           Data.Bifunctor
import           Data.Maybe
import           Lang
import           LExpr
import           Static.Impl
import           Static.Resolve
import           Static.Store
import           Value

data TranspileError
  = ResolveError ResolveError
  | LangNotSupported Lang
  deriving (Eq, Show)

transpile :: Store -> Lang -> LExpr Value -> Either TranspileError String
transpile store lang lexpr = resolve' lexpr >>= transpile'
  where
    resolve' = first ResolveError . resolve store
    transpileOne name impl =
      maybeRight (LangNotSupported lang) (tosrc lang impl >>= bind lang name)
    transpile' (Resolved expr bindings) = do
      deps <-
        sequence [transpileOne id (impl item) | (Binding id item) <- bindings]
      expr <- maybeRight (LangNotSupported lang) (tosrc lang (LalaImpl [] expr))
      return $ unlines (deps ++ [outputFun lang expr])
    maybeRight err = maybe (Left err) Right

-- | Language specific conversion of a value to the expected format.
-- For now - simply printing to stdout will do.
-- TODO: handle more cases - noop, exports, etc.
outputFun :: Lang -> String -> String
outputFun Js impl = "console.log(" ++ impl ++ ")"
