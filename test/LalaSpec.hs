{-# LANGUAGE OverloadedLists #-}

-- | Tests for top level functionality.
module LalaSpec
  ( spec
  ) where

import           Lala
import           LalaType   (fromString, singletonT)
import           LExpr
import           Refs
import           Test.Hspec
import           Type
import           Value

parseT s = either (error s) id (fromString s)

refs =
  [ ("Add", RefData (parseT "CNum a => a -> a -> a"))
  , ("Show", RefData (parseT "CNum a, CStr b => a -> b"))
  ]

spec :: Spec
spec = do
  describe "infer ValueLExpr" $ do
    let infer' = inferValueLExpr refs
    it "sequential application" $
      infer' (ref "Add" |< intLiteral 1 |< intLiteral 1) `shouldBe`
      Right (singletonT CNum)
    it "nested application" $
      infer'
        (ref "Add" |< (ref "Add" |< intLiteral 1 |< intLiteral 2) |<
         intLiteral 3) `shouldBe`
      Right (singletonT CNum)
    it "type transition" $
      infer' (ref "Show" |< intLiteral 1) `shouldBe` Right (singletonT CStr)
