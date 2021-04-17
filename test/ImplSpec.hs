{-# LANGUAGE QuasiQuotes #-}

module ImplSpec
  ( spec
  ) where

import Text.RawString.QQ
import Impl
import Test.Hspec

import TestingUtils (parseTypeOrDie)

spec :: Spec
spec =
  describe "implToStr" $ do
    it "PyLambda" $
      implToStr
        "add"
        (Impl
           (parseTypeOrDie "CAny a => a -> a -> a")
           (pyLambda ["x, y"] "x + y")) `shouldBe`
      "add = lambda x, y: x + y"
    it "PyDef" $
      implToStr
        "add"
        (Impl
           (parseTypeOrDie "CAny a => a -> a -> a")
           (pyDef ["x", "y"] ["ans = x + y", "return ans"])) `shouldBe` [r|def add(arg):
    def wrapper():
        def inner(x):
            def inner(y):
                ans = x + y
                return ans
            return inner
        return inner
    return wrapper () (arg)
|]
    it "PyLiteral" $
      implToStr "x" (Impl (parseTypeOrDie "CAny a => a") (pyLiteral "[1, 2]")) `shouldBe`
      "x = [1, 2]"
    it "PyImport" $
      implToStr
        "foldl"
        (Impl
           (parseTypeOrDie "CAny a, CAny b => (a -> a) -> b -> a")
           (pyImport ["functools"] "reduce")) `shouldBe`
      "from functools import reduce as foldl"
