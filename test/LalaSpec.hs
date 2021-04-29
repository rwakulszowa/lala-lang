{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | Tests for top level functionality.
module LalaSpec
  ( spec
  ) where

import qualified Data.Text         as T
import           Lala
import           LalaType          (singletonT)
import           Lang
import           LExpr
import           System.Exit
import           System.Process
import           Test.Hspec
import           Testing
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

spec :: Spec
spec = do
  describe "process" $ do
    let go e =
          case process store Js e of
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
      (`shouldBe` Right (parseT "CMaybe m, Nil a => m a", "undefined"))
