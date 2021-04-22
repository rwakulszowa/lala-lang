{-# LANGUAGE OverloadedLists #-}

module Static.ResolveSpec
  ( spec
  ) where

import           Data.Map
import           LExpr
import           Static.Resolve
import           Test.Hspec
import           Testing
import           Type

getBinding k = Binding k (store ! k)

spec :: Spec
spec = do
  describe "resolve" $ do
    it "unknown ref" $ resolve store (ref "x") `shouldBe` Left (RefNotFound "x")
    it "application with nested dependencies" $ do
      let e = ref "Inc" |< intLiteral 2
      resolve store e `shouldBe`
        Right
          (Resolved {expr = e, bindings = [getBinding "Add", getBinding "Inc"]})
    it "same dependency on multiple levels" $
      -- | Reproduces an old issue where a dependency would be pushed to the end
      -- of the list, causing an unresolved dependency error.
     do
      let e = ref "Add" |< intLiteral 1 |< (ref "Inc" |< intLiteral 2)
      resolve store e `shouldBe`
        -- The order is important here. Inc depends on Add, therefore Add should make
        -- it to the top of the list.
        Right
          (Resolved {expr = e, bindings = [getBinding "Add", getBinding "Inc"]})
