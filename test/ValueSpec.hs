{-# LANGUAGE OverloadedStrings #-}

module ValueSpec
  ( spec
  ) where

import           Data.Parse
import           LExpr
import           Test.Hspec
import           Value

spec :: Spec
spec = do
  describe "parse" $ do
    it "string literal" $ parseString "\"A\"" `shouldBe` Right (strLiteral "A")
  describe "unparse" $ do
    it "string literal" $ unparse (strLiteral "A") `shouldBe` "\"A\""
