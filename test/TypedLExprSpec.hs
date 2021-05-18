{-# LANGUAGE OverloadedStrings #-}

module TypedLExprSpec
  ( spec
  ) where

import           Data.List.NonEmpty
import           Data.Parse
import           LExpr
import           LalaType           as LT
import           Test.Hspec
import           Type
import           TypedLExpr
import           Typiara.FT
import           Value

spec :: Spec
spec = do
  describe "parseTLE" $ do
    it "literal" $
      parseTLE "CNum a => a : 1" `shouldBe`
      Right (TypedLExpr {typ = LT.singleton (T CNum), expr = intLiteral 1})
    it "application" $
      parseTLE "CNum a => a : Inc 1" `shouldBe`
      Right
        (TypedLExpr
           {typ = LT.singleton (T CNum), expr = ref "Inc" |< intLiteral 1})
