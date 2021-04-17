{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module TranspileSpec
  ( spec
  ) where

import Expression
import Lang
import Parse
import ProcessedExpression
import Test.Hspec
import TestingUtils
import Text.RawString.QQ
import Transpile

spec :: Spec
spec = do
  let adin = ImplementationExpression $ intLiteral 1
  let dwa = ImplementationExpression $ intLiteral 2
  let inc =
        FunctionExpression
          ["x"]
          (applicationExpression
             (externalRef "Add")
             [intLiteral 1, localRef "x"])
  let add12 =
        ImplementationExpression
          (applicationExpression
             (externalRef "Add")
             [intLiteral 1, intLiteral 2])
  let addXY =
        FunctionExpression
          ["x", "y"]
          (applicationExpression
             (externalRef "Add")
             [localRef "y", localRef "x"])
  let addNested =
        ImplementationExpression $
        applicationExpression
          (externalRef "Add")
          [ intLiteral 1
          , applicationExpression
              (externalRef "Add")
              [intLiteral 1, intLiteral 1]
          ]
  let inlineId = FunctionExpression ["x"] (localRef "x")
  describe "(transpileTo Py)" $ do
    it "Add" $ transpileTo Py add12 `shouldBe` "((Add (1)) (2))"
    it "Add y x" $
      transpileTo Py addXY `shouldBe` "lambda x: lambda y: ((Add (y)) (x))"
    it "Inc" $ transpileTo Py inc `shouldBe` "lambda x: ((Add (1)) (x))"
    it "Adin" $ transpileTo Py adin `shouldBe` "1"
    it "AddNested" $
      transpileTo Py addNested `shouldBe` "((Add (1)) (((Add (1)) (1))))"
    it "InlineId" $ transpileTo Py inlineId `shouldBe` "lambda x: x"
  describe "(transpileTo Js)" $ do
    it "Add" $ transpileTo Js add12 `shouldBe` "((Add (1)) (2))"
    it "Add y x" $ transpileTo Js addXY `shouldBe` "x => y => ((Add (y)) (x))"
    it "Inc" $ transpileTo Js inc `shouldBe` "x => ((Add (1)) (x))"
    it "Adin" $ transpileTo Js adin `shouldBe` "1"
    it "AddNested" $
      transpileTo Js addNested `shouldBe` "((Add (1)) (((Add (1)) (1))))"
    it "InlineId" $ transpileTo Js inlineId `shouldBe` "x => x"
  describe "(expr2Lala)" $ do
    it "Add" $ expr2Lala add12 `shouldBe` "((Add 1) 2)"
    it "Add y x" $ expr2Lala addXY `shouldBe` "x y -> ((Add y) x)"
    it "Inc" $ expr2Lala inc `shouldBe` "x -> ((Add 1) x)"
    it "Adin" $ expr2Lala adin `shouldBe` "1"
    it "AddNested" $ expr2Lala addNested `shouldBe` "((Add 1) ((Add 1) 1))"
