{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | Tests for top level functionality.
-- Use the real HardcodedStore.
module LalaSpec
  ( spec
  ) where

import           Data.Maybe
import qualified Data.Text             as T
import           LExpr
import           Lala
import           LalaType
import           Lang
import           Static.HardcodedStore
import           Static.Impl
import           Static.Store
import           System.Exit
import           System.Process
import           Test.Hspec
import           Testing               (parseT)
import           Text.RawString.QQ
import           Type
import           Value

eval :: T.Text -> IO (Either String String)
eval content = do
  (exitCode, stdout, stderr) <-
    readProcessWithExitCode "node" [] (T.unpack content)
  return $
    if exitCode == ExitSuccess
      -- Strip the trailing newline.
      then Right (init stdout)
      else Left (show (exitCode, stderr, content))

store' =
  store <>
  [ ( "Reorder_10"
    , Item {typ = fromJust (reorderT [1, 0]), impl = fromJust (reorderF [1, 0])})
  , ( "Reorder_201"
    , Item
        { typ = fromJust (reorderT [2, 0, 1])
        , impl = fromJust (reorderF [2, 0, 1])
        })
  ]

spec :: Spec
spec = do
  describe "process" $ do
    let go e =
          case process store' Js e of
            (Right (typ, src)) -> do
              ret <- eval src
              return $ (typ, ) <$> ret
            (Left procerr) -> return $ Left (show procerr)
    it "single application" $
      go (ref "Inc" |< intLiteral 1) >>=
      (`shouldBe` Right (singletonT CNum, "2"))
    it "nested application" $
      go
        (ref "Add" |< (ref "Add" |< intLiteral 1 |< intLiteral 2) |<
         intLiteral 3) >>=
      (`shouldBe` Right (singletonT CNum, "6"))
    it "type transition" $
      go (ref "Show" |< intLiteral 1) >>=
      (`shouldBe` Right (singletonT CStr, "1"))
    it "Maybe" $
      go (ref "Head" |< ref "Nil") >>=
      (`shouldBe` Right (parseT "CMaybe m, Nil a => m a", "null"))
    it "Pair" $
      go
        (ref "First" |< ref "Inc" |<
         (ref "Pair" |< intLiteral 1 |< strLiteral "abc")) >>=
      (`shouldBe` Right
                    (parseT "CProd p, CNum a, CStr b => p a b", "[ 2, 'abc' ]"))
    describe "Reorder" $
      -- Validate that reorder function and implementation are in sync.
     do
      it "Const, [1, 0]" $
        go
          ((ref "Reorder_10" |< ref "Const") |< intLiteral 1 |< strLiteral "abc") >>=
        (`shouldBe` Right (parseT "CStr a => a", "abc"))
      it "Foldl, [2, 0, 1]" $
        go
          ((ref "Reorder_201" |< ref "Foldl") |< ref "Nil" |< ref "Add" |<
           intLiteral 0) >>=
        (`shouldBe` Right (parseT "CNum a => a", "0"))
