module LExprSpec
  ( spec
  ) where

import           LalaType   (fromString, singletonT)
import           LExpr
import           Test.Hspec
import           Type

spec :: Spec
spec = do
  describe "inferLExpr" $ do
    it "singleton" $
      inferLExpr (singleton (singletonT CNum)) `shouldBe`
      Right (singletonT CNum)
    it "application" $ do
      let tN = singletonT CNum
      let (Right tNNN) = fromString "CNum a => a -> a -> a"
      inferLExpr (singleton tNNN |< singleton tN |< singleton tN) `shouldBe`
        Right tN
