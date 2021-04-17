module Data.BiDiEqMapSpec
  ( spec
  ) where

import Data.BiDiEqMap
import Test.Hspec

spec :: Spec
spec = do
  describe "lookupByKey" $ do
    it "empty" $
      0 `lookupByKey` (fromList [] :: BiDiEqMap Char) `shouldBe` Nothing
    it "nonempty" $ do
      let obj = fromList "ABC"
      ((`lookupByKey` obj) <$> [-1 .. 3]) `shouldBe`
        [Nothing, Just 'A', Just 'B', Just 'C', Nothing]
    it "duplicates" $ do
      let obj = fromList "ABA"
      ((`lookupByKey` obj) <$> [0 .. 2]) `shouldBe`
        [Just 'A', Just 'B', Nothing]
  describe "lookupByValue" $ do
    it "empty" $
      'A' `lookupByValue` (fromList [] :: BiDiEqMap Char) `shouldBe` Nothing
    it "nonempty" $ do
      let obj = fromList "ABC"
      ((`lookupByValue` obj) <$> "XABCD") `shouldBe`
        [Nothing, Just 0, Just 1, Just 2, Nothing]
    it "duplicates" $ do
      let obj = fromList "ABA"
      ((`lookupByValue` obj) <$> "ABC") `shouldBe` [Just 0, Just 1, Nothing]
