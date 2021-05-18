{-# LANGUAGE OverloadedStrings #-}

module LExprSpec
  ( spec
  ) where

import           Data.Parse
import           LExpr
import           LalaType   (fromString, singletonT)
import           Test.Hspec
import           Type

spec :: Spec
spec = do
  describe "unparse" $ do
    it "application" $ unparse (ref "Inc" |< intLiteral 1) `shouldBe` "Inc 1"
  describe "inferLExpr" $ do
    it "singleton" $
      inferLExpr (singleton (singletonT CNum)) `shouldBe`
      Right (singletonT CNum)
    it "application" $ do
      let tN = singletonT CNum
      let (Right tNNN) = fromString "CNum a => a -> a -> a"
      inferLExpr (singleton tNNN |< singleton tN |< singleton tN) `shouldBe`
        Right tN
