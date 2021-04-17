{-# LANGUAGE OverloadedLists #-}

module TypeCheckSpec
  ( spec
  ) where

import Data.Map (Map(..))

import qualified Data.Map.Strict as Map
import qualified LalaType

import Impl
import LalaType (LalaType(..))
import PieceOfLogic
import ProcessedExpression (ProcessedExpression(..), fromExpression)
import Test.Hspec
import TestingUtils
import Type (Type(..))
import TypeCheck
import Typiara.FT (FT(..))

unwrap :: Either a b -> b
unwrap (Right b) = b

procExprOrDie = fromExpression . parseExpressionOrDie

addImpl = Impl addTypeDef (pyLambda ["x", "y"] "x + y")

incImpl = Impl incTypeDef (pyLambda ["x"] "x + 1")

spec :: Spec
spec = do
  describe "typeCheckExpression" $ do
    it "literal constant" $
      typeCheckExpression
        []
        (LalaType.singleton (T CNum))
        (parseExpressionOrDie "1") `shouldBe`
      Right ()
    it "flat generic function with no deps" $
      typeCheckExpression
        []
        (parseTypeOrDie "Nil a => a -> a")
        (parseExpressionOrDie "x -> x") `shouldBe`
      Right ()
    it "generic HOF" $
      typeCheckExpression
        []
        (parseTypeOrDie "Nil a, Nil b => (a -> b) -> a -> b")
        (parseExpressionOrDie "f x -> (f x)") `shouldBe`
      Right ()
    it "partial application with a direct dep" $
      typeCheckExpression
        [("Add", addTypeDef)]
        (parseTypeOrDie "CNum a => a -> a")
        (parseExpressionOrDie "(Add 1)") `shouldBe`
      Right ()
    it "application with a direct dep" $
      typeCheckExpression
        [("Add", addTypeDef)]
        (LalaType.singleton (T CNum))
        (parseExpressionOrDie "((Add 1) 1)") `shouldBe`
      Right ()
    it "application with nested deps" $
      typeCheckExpression
        [("Add", addTypeDef), ("Inc", incTypeDef)]
        (LalaType.singleton (T CNum))
        (parseExpressionOrDie "(Inc 1)") `shouldBe`
      Right ()
    it "function with nested application" $
      typeCheckExpression
        [("Add", addTypeDef), ("Inc", incTypeDef)]
        (incTypeDef)
        (parseExpressionOrDie "x -> ((Add (Inc x)) 1)") `shouldBe`
      Right ()
  describe "typeCheckPiece" $ do
    it "impl piece" $ typeCheckPiece [] (ImplPiece addImpl) `shouldBe` Right ()
    it "expr piece" $
      typeCheckPiece [] (ExprPiece ((procExprOrDie "1" numTypeDef))) `shouldBe`
      Right ()
  describe "typeCheckResolvedExpression" $ do
    it "no dependencies" $
      typeCheckResolvedExpression [] ((procExprOrDie "1" numTypeDef)) `shouldBe`
      Right ()
    it "single dependency" $
      typeCheckResolvedExpression
        [("Add", ImplPiece addImpl)]
        ((procExprOrDie "(Add 1)" incTypeDef)) `shouldBe`
      Right ()
    it "self cyclic dependency" $ do
      let depAA = ("A", ExprPiece $ ((procExprOrDie "(A 1)" numTypeDef)))
      typeCheckResolvedExpression [depAA] ((procExprOrDie "(A 2)" numTypeDef)) `shouldBe`
        (Left $ DependencyCycle ["A", "A"])
    it "mutually cyclic dependencies" $ do
      let depAB = ("A", ExprPiece ((procExprOrDie "(B 1)" numTypeDef)))
      let depBA = ("B", ExprPiece ((procExprOrDie "(A 2)" numTypeDef)))
      typeCheckResolvedExpression
        [depAB, depBA]
        ((procExprOrDie "(A 3)" numTypeDef)) `shouldBe`
        (Left $ DependencyCycle ["A", "B", "A"])
