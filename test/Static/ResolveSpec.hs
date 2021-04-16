{-# LANGUAGE OverloadedLists #-}

module Static.ResolveSpec
  ( spec
  ) where

import           Data.Map
import           LalaType       (fromString)
import           LExpr
import           Static.Impl
import           Static.Resolve
import           Static.Store
import           Test.Hspec
import           Type

parseT s = either (error s) id (fromString s)

store =
  [ ( "Add"
    , Item
        { typ = parseT "CNum a => a -> a -> a"
        , impl = JsLambda ["x", "y"] "x + y"
        })
  , ( "Inc"
    , Item
        { typ = parseT "CNum a => a -> a -> a"
        , impl = LalaImpl ["x"] (ref "Add" |< intLiteral 1 |< ref "x")
        })
  ]

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
