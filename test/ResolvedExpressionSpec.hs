{-# LANGUAGE OverloadedLists #-}

module ResolvedExpressionSpec
  ( spec
  ) where

import qualified Data.Either         as Either
import           Data.Map            (Map)
import qualified Data.Map.Strict     as Map
import           Impl
import           PieceOfLogic
import           ProcessedExpression
import           ResolvedExpression
import           Test.Hspec
import           TestingUtils

buildLookup :: [(String, PieceOfLogic)] -> String -> [PieceOfLogic]
buildLookup extItems k =
  let assocs = [(k, [v]) | (k, v) <- extItems]
   in Map.findWithDefault [] k $ Map.fromListWith mappend assocs

unpack :: ResolvedExpression -> (ProcessedExpression, Map String PieceOfLogic)
unpack re = (procExpr re, boundRefs re)

spec :: Spec
spec = do
  let add =
        ImplPiece $
        Impl
          { typ = parseTypeOrDie "Nil a => a -> a -> a"
          , src = pyLambda ["x", "y"] "x + y"
          }
  let one = ExprPiece $ fromExpression (parseExpressionOrDie "1") numTypeDef
  let one' =
        ImplPiece $
        Impl {typ = parseTypeOrDie "CNum a => a", src = pyLiteral "1"}
  let inc =
        ExprPiece $
        fromExpression (parseExpressionOrDie "x -> ((Add 1) x)") incTypeDef
  let inc' =
        ExprPiece $
        fromExpression (parseExpressionOrDie "x -> ((Add x) 1)") incTypeDef
  describe "resolvedExpression" $ do
    it "minimal valid input" $ do
      let procExpr = fromExpression (parseExpressionOrDie "1") numTypeDef
      (unpack <$> resolvedExpression procExpr mempty) `shouldBe`
        Right (procExpr, mempty)
    it "missing binding" $ do
      let procExpr =
            fromExpression (parseExpressionOrDie "(RefA RefB)") numTypeDef
      (unpack <$> resolvedExpression procExpr [("RefA", one)]) `shouldBe`
        Left (UnboundRefs ["RefB"])
    it "conflicting types" $ do
      let procExpr =
            fromExpression (parseExpressionOrDie "(RefA RefB)") numTypeDef
      (unpack <$> resolvedExpression procExpr [("RefA", one'), ("RefB", one')]) `shouldSatisfy`
        Either.isLeft
  describe "resolve" $ do
    it "ImplPiece" $ do
      let procExpr = fromExpression (parseExpressionOrDie "One") numTypeDef
      (fmap unpack <$> resolve (buildLookup [("One", one)]) procExpr) `shouldBe`
        Right [(procExpr, [("One", one)])]
    it "ExprPiece" $ do
      let procExpr = fromExpression (parseExpressionOrDie "Inc") incTypeDef
      (fmap unpack <$>
       resolve (buildLookup [("Inc", inc), ("Add", add)]) procExpr) `shouldBe`
        Right [(procExpr, [("Add", add), ("Inc", inc)])]
    it "application with nested dependency" $ do
      let procExpr = fromExpression (parseExpressionOrDie "(Inc 1)") numTypeDef
      fmap unpack <$>
        resolve (buildLookup [("Inc", inc), ("Add", add)]) procExpr `shouldBe`
        Right [(procExpr, [("Add", add), ("Inc", inc)])]
    it "application, multple matching impls" $ do
      let procExpr =
            fromExpression (parseExpressionOrDie "(Inc One)") numTypeDef
      (fmap unpack <$>
       resolve
         (buildLookup
            [ ("Inc", inc)
            , ("Inc", inc')
            , ("Add", add)
            , ("One", one)
            , ("One", one')
            ])
         procExpr) `shouldBe`
        Right
          [ (procExpr, [("Add", add), ("Inc", inc'), ("One", one')])
          , (procExpr, [("Add", add), ("Inc", inc'), ("One", one)])
          , (procExpr, [("Add", add), ("Inc", inc), ("One", one')])
          , (procExpr, [("Add", add), ("Inc", inc), ("One", one)])
          ]
    it "two matching impls" $ do
      let procExpr = fromExpression (parseExpressionOrDie "One") numTypeDef
      fmap unpack <$>
        resolve (buildLookup [("One", one), ("One", one')]) procExpr `shouldBe`
        Right [(procExpr, [("One", one')]), (procExpr, [("One", one)])]
    it "unknown ident" $ do
      let procExpr =
            fromExpression (parseExpressionOrDie "UnknownIdent") numTypeDef
      resolve (buildLookup [("One", one)]) procExpr `shouldBe`
        Left (LookupFailed "UnknownIdent")
    it "missing dependency" $ do
      let procExpr = fromExpression (parseExpressionOrDie "Inc") numTypeDef
      resolve (buildLookup [("Inc", inc)]) procExpr `shouldBe`
        Left (LookupFailed "Add")
