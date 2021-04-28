module Main where

import           Data.Bifunctor
import           Data.Parse
import qualified Data.Text             as T
import           Lala
import           Static.HardcodedStore (store)
import           System.Environment

-- | Transpile an expression into Js.
--
-- Sample usage:
-- >>> echo "Len (Cons 1 Nil)" | stack run | node -
-- >>> 1
main :: IO ()
main = do
  contents <- getContents
  putStrLn (handle contents)
  where
    handle s = print $ parseString s >>= (coerceErr . process store Js)
    print (Right (_, src)) = T.unpack src
    print (Left e)         = "Error: " ++ e
    coerceErr = first show
