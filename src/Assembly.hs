module Assembly
  ( Assembly(..)
  , build
  ) where

import qualified Impl
import qualified Lang
import           PieceOfLogic (PieceOfLogic)
import           Transpile    (bind, dumpLogic)

data Assembly
    -- | Single file result.
    -- No exports - the file is expected to be executed as a whole, throudh stdin.
      =
  InlineAssembly
    { content :: String
    , lang    :: Lang.Lang
    }
  deriving (Show, Eq)

kReturnId = "ret"

-- | Turn a set of pieces (assignments / definitions) into a single file.
-- The last piece is expected to be the result. It's bound to a value of `
build :: Lang.Lang -> [(String, PieceOfLogic)] -> Assembly
build Lang.Py pieces =
  let retId = fst . last $ pieces
      content = dumpLogic Lang.Py pieces ++ "\n" ++ bind Lang.Py kReturnId retId
   in InlineAssembly content Lang.Py
build Lang.Js pieces =
  let retId = fst . last $ pieces
      content = dumpLogic Lang.Js pieces ++ "\n" ++ bind Lang.Js kReturnId retId
   in InlineAssembly content Lang.Js
