{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Static.ImplSpec
  ( spec
  ) where

import           LalaType    (fromString, singletonT)
import           Lang
import           LExpr
import           Static.Impl
import           Test.Hspec
import           Type
import           Value

spec :: Spec
spec = do
  describe "tosrc" $ do
    describe "Js" $ do
      it "singleton" $ tosrc Js (LalaImpl [] (intLiteral 1)) `shouldBe` Just "1"
      it "application" $
        tosrc Js (LalaImpl ["x", "y"] (ref "Add" |< ref "x" |< ref "y")) `shouldBe`
        Just "x => y => Add(x) (y)"
      it "nested application" $
        tosrc
          Js
          (LalaImpl ["x"] (ref "Add" |< (ref "Inc" |< intLiteral 1) |< ref "x")) `shouldBe`
        Just "x => Add(Inc(1)) (x)"
    describe "Lala" $ do
      it "application" $
        tosrc Lala (LalaImpl ["x", "y"] (ref "Add" |< ref "x" |< ref "y")) `shouldBe`
        Just "x y -> (Add x y)"
