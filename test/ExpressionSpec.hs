module ExpressionSpec
  ( spec
  ) where

import           Data.BinaryTree
import           Data.Char       (toUpper)
import           Expression
import           Test.Hspec

spec :: Spec
spec = do
  describe "digExternalRefs" $ do
    describe "ImplementationExpression" $ do
      it "single external ref" $
        digExternalRefs (ImplementationExpression $ externalRef "A") `shouldBe`
        ["A"]
      it "nested, mixed" $
        digExternalRefs
          (ImplementationExpression $
           Node (externalRef "A") (Node (localRef "a") (externalRef "B"))) `shouldBe`
        ["A", "B"]
    describe "FunctionExpression" $ do
      it "single external ref" $
        digExternalRefs
          (FunctionExpression ["arg_0", "arg_1"] (externalRef "A")) `shouldBe`
        ["A"]
  describe "applicationExpression" $ do
    it "singleton" $ do
      let x = intLiteral 1
      applicationExpression x [] `shouldBe` x
    it "two arguments" $
      -- | (f a) b
     do
      let f = externalRef "f"
      let a = externalRef "a"
      let b = externalRef "b"
      applicationExpression f [a, b] `shouldBe` Node (Node f a) b
  describe "fmapExternalRefs" $ do
    let up = map toUpper
    it "singleton" $
      (up `fmapExternalRefs` ImplementationExpression (externalRef "x")) `shouldBe`
      ImplementationExpression (externalRef "X")
    it "Node" $
      (up `fmapExternalRefs`
       ImplementationExpression (Node (externalRef "f") (localRef "a"))) `shouldBe`
      ImplementationExpression (Node (externalRef "F") (localRef "a"))
