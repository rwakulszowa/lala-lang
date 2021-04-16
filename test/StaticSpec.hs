{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes     #-}

-- | Top level-ish tests for the static part of the language.
-- Dependency resolution and transpilation tests.
module StaticSpec
  ( spec
  ) where

import           Lang
import           Static
import           Test.Hspec
import           Testing
import           Text.RawString.QQ

spec :: Spec
spec = do
  describe "transpile" $ do
    describe "JS" $ do
      let t = transpile store Js . parseE
      it "literal" $ t "1" `shouldBe` Right "console.log(1)\n"
      it "with deps" $
        t "Inc 2" `shouldBe`
        Right
          [r|const Add = x => y => x + y
const Inc = x => Add(1) (x)
console.log(Inc(2))
|]
