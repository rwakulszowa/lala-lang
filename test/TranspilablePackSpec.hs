{-# LANGUAGE QuasiQuotes, OverloadedLists #-}

module TranspilablePackSpec
  ( spec
  ) where

import Assembly
import Data.BinaryTree
import Data.Either (isLeft)
import Data.Map.Strict
import Expression (Literal(..))
import Impl
import Lang (Lang(..))
import Parse (parseExpression)
import PieceOfLogic
import ProcessedExpression
import Test.Hspec
import TestingUtils
import Text.RawString.QQ
import TranspilablePack

addImplPy =
  ImplPiece $
  Impl (parseTypeOrDie "CNum a => a -> a -> a") (pyLambda ["x", "y"] "x + y")

addImplJs =
  ImplPiece $
  Impl (parseTypeOrDie "CNum a => a -> a -> a") (jsLambda ["x", "y"] "x + y")

incImpl =
  ExprPiece
    ((fromExpression (parseExpressionOrDie "x -> ((Add 1) x)") incTypeDef))

applyImpl =
  ExprPiece
    ((fromExpression
        (parseExpressionOrDie "fun arg -> (fun arg)")
        (parseTypeOrDie "Nil a, Nil b => (a -> b) -> a -> b")))

spec :: Spec
spec =
  describe "transpilePack" $ do
    describe "Py" $ do
      it "Add 1 1" $
        transpilePack
          ([("Add", [addImplPy])] !)
          (TranspilablePack
             (((Leaf $ Right $ "Add") `Node` (Leaf $ Left $ IntLiteral 1)) `Node`
              (Leaf $ Left $ IntLiteral 1))
             Py
             "a.py") `shouldBe`
        Right
          [ InlineAssembly
              [r|Add = lambda x: lambda y: x + y
Ret = ((Add (1)) (1))


ret = Ret|]
              Py
          ]
      it "Inc(1)" $
        transpilePack
          ([("Add", [addImplPy]), ("Inc", [incImpl])] !)
          (TranspilablePack
             ((Leaf $ Right $ "Inc") `Node` (Leaf $ Left $ IntLiteral 1))
             Py
             "a.py") `shouldBe`
        Right
          [ InlineAssembly
              [r|Add = lambda x: lambda y: x + y
Inc = lambda x: ((Add (1)) (x))

Ret = (Inc (1))


ret = Ret|]
              Py
          ]
      it "Add(2, Inc(1))" $
        transpilePack
          ([("Add", [addImplPy]), ("Inc", [incImpl])] !)
          (TranspilablePack
             (((Leaf $ Right $ "Add") `Node` (Leaf $ Left $ IntLiteral 2)) `Node`
              ((Leaf $ Right $ "Inc") `Node` (Leaf $ Left $ IntLiteral 1)))
             Py
             "a.py") `shouldBe`
        Right
          [ InlineAssembly
              [r|Add = lambda x: lambda y: x + y
Inc = lambda x: ((Add (1)) (x))

Ret = ((Add (2)) ((Inc (1))))


ret = Ret|]
              Py
          ]
      it "HOF with lambda" $
        transpilePack
          ([("Add", [addImplPy]), ("Inc", [incImpl]), ("Apply", [applyImpl])] !)
          (TranspilablePack
             (((Leaf $ Right $ "Apply") `Node` (Leaf $ Right $ "Inc")) `Node`
              (Leaf $ Left $ IntLiteral 1))
             Py
             "a.py") `shouldBe`
        Right
          [ InlineAssembly
              [r|Add = lambda x: lambda y: x + y
Apply = lambda fun: lambda arg: (fun (arg))

Inc = lambda x: ((Add (1)) (x))

Ret = ((Apply (Inc)) (1))


ret = Ret|]
              Py
          ]
      it "string with special chars" $
        transpilePack
          mempty
          (TranspilablePack (Leaf $ Left $ StrLiteral "\"c=3\"") Py "a.py") `shouldBe`
        Right
          [ InlineAssembly
              [r|Ret = "\"c=3\""


ret = Ret|]
              Py
          ]
      it "import" $
        transpilePack
          ([ ( "Imported"
             , [ ImplPiece $
                 Impl
                   (parseTypeOrDie "Nil a => a -> a")
                   (pyImport ["some", "path"] "fun")
               ])
           ] !)
          (TranspilablePack
             ((Leaf $ Right $ "Imported") `Node` (Leaf $ Left $ IntLiteral 1))
             Py
             "a.py") `shouldBe`
        Right
          [ InlineAssembly
              [r|from some.path import fun as Imported
Ret = (Imported (1))


ret = Ret|]
              Py
          ]
    describe "Js" $
      it "Add 1 1" $
      transpilePack
        ([("Add", [addImplJs])] !)
        (TranspilablePack
           (((Leaf $ Right $ "Add") `Node` (Leaf $ Left $ IntLiteral 1)) `Node`
            (Leaf $ Left $ IntLiteral 1))
           Js
           "lib.mjs") `shouldBe`
      Right
        [ InlineAssembly
            [r|const Add = x => y => x + y
const Ret = ((Add (1)) (1))


const ret = Ret|]
            Js
        ]
