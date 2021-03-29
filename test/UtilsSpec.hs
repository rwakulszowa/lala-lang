module UtilsSpec
  ( spec
  ) where

import           Test.Hspec
import           Utils

import qualified Data.Char
import qualified Data.Map.Strict as Map

import           Data.Map        (Map)

spec :: Spec
spec = do
  describe "replaceValues" $ do
    it "empty" $ replaceValues [0 ..] (mempty :: String) `shouldBe` mempty
    it "singleton" $
      replaceValues [0 ..] ['a'] `shouldBe` (Map.singleton 'a' 0, [0])
    it "multiple values with overlap" $
      replaceValues [0 ..] ['a', 'b', 'a', 'c'] `shouldBe`
      (Map.fromList [('a', 0), ('b', 1), ('c', 2)], [0, 1, 0, 2])
  describe "unionMapsRejectOverlap" $ do
    it "empty list" $
      unionMapsRejectOverlap (mempty :: [Map Char Int]) `shouldBe` Right mempty
    it "two empty maps" $
      unionMapsRejectOverlap ([mempty, mempty] :: [Map Char Int]) `shouldBe`
      Right mempty
    it "no conflicts" $
      unionMapsRejectOverlap [Map.fromList [('a', 0)], Map.fromList [('b', 1)]] `shouldBe`
      Right (Map.fromList [('a', 0), ('b', 1)])
    it "conflict" $
      unionMapsRejectOverlap [Map.fromList [('a', 0)], Map.fromList [('a', 1)]] `shouldBe`
      Left 'a'
  describe "intoUniqueIdMap" $ do
    it "empty" $ intoUniqueIdMap "" `shouldBe` mempty
    it "duplicates" $
      intoUniqueIdMap "aba" `shouldBe` Map.fromList [(0, 'a'), (1, 'b')]
