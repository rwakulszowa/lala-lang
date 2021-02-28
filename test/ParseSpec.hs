module ParseSpec
  ( spec
  ) where

import Data.BinaryTree
import Data.Either (isLeft)
import Data.List.NonEmpty
import Expression
import Parse
import Test.Hspec

spec :: Spec
spec = do
  describe "parseDynamicExpression" $ do
    it "flat" $
      parseDynamicExpression "(Inc 1)" `shouldBe`
      Right (Node (Leaf $ Right "Inc") (Leaf $ Left $ IntLiteral 1))
    it "nested" $
      parseDynamicExpression "((Add (Inc 1)) \"a\")" `shouldBe`
      Right
        (Node
           (Node
              (Leaf $ Right "Add")
              (Node (Leaf $ Right "Inc") (Leaf $ Left $ IntLiteral 1)))
           (Leaf $ Left $ StrLiteral "a"))
  describe "parseExpression" $ do
    it "shortened literal" $
      parseExpression "1" `shouldBe`
      Right (ImplementationExpression $ intLiteral 1)
    it "function with no args" $ parseExpression "-> 1" `shouldSatisfy` isLeft
    it "string literal" $
      parseExpression "\"abc\"" `shouldBe`
      Right (ImplementationExpression $ strLiteral "abc")
    it "simple function" $
      parseExpression "x -> ((Add 1) x)" `shouldBe`
      Right
        (FunctionExpression
           ["x"]
           (Node (Node (externalRef "Add") (intLiteral 1)) (localRef "x")))
    it "nested applications" $ do
      let add = externalRef "Add"
      parseExpression "x -> ((Add 1) ((Add 2) ((Add 3) x)))" `shouldBe`
        Right
          (FunctionExpression
             ["x"]
             (applicationExpression
                add
                [ intLiteral 1
                , applicationExpression
                    add
                    [ intLiteral 2
                    , applicationExpression add [intLiteral 3, localRef "x"]
                    ]
                ]))
  describe "parseType" $ do
    it "function with one constraint" $
      parseType "Num a => a -> a" `shouldBe`
      Right
        (ParsedType
           [ParsedConstraint (TypeTag "Num") (TypeVarId "a")]
           (SignatureNode
              (SignatureLeaf (TypeVarId "a" :| []))
              (SignatureLeaf (TypeVarId "a" :| []))))
    it "no constraints" $
      parseType "=> x" `shouldBe`
      Right (ParsedType [] (SignatureLeaf (TypeVarId "x" :| [])))
    it "type application" $
      parseType "=> s a" `shouldBe`
      Right (ParsedType [] (SignatureLeaf (TypeVarId "s" :| [TypeVarId "a"])))
    it "multiple constraints" $
      parseType "Num a, Show b => a -> b" `shouldBe`
      Right
        (ParsedType
           [ ParsedConstraint (TypeTag "Num") (TypeVarId "a")
           , ParsedConstraint (TypeTag "Show") (TypeVarId "b")
           ]
           (SignatureNode
              (SignatureLeaf (TypeVarId "a" :| []))
              (SignatureLeaf (TypeVarId "b" :| []))))
    it "function with arity > 1" $
      parseType "=> a -> b -> c -> d" `shouldBe`
      Right
        (ParsedType
           []
           (SignatureNode
              (SignatureLeaf (TypeVarId "a" :| []))
              (SignatureNode
                 (SignatureLeaf (TypeVarId "b" :| []))
                 (SignatureNode
                    (SignatureLeaf (TypeVarId "c" :| []))
                    (SignatureLeaf (TypeVarId "d" :| []))))))
    it "hof" $
      parseType "=> (a -> b) -> c" `shouldBe`
      Right
        (ParsedType
           []
           (SignatureNode
              (SignatureNode
                 (SignatureLeaf (TypeVarId "a" :| []))
                 (SignatureLeaf (TypeVarId "b" :| [])))
              (SignatureLeaf (TypeVarId "c" :| []))))
    it "right side parens" $ parseType "=> a -> (b -> c)" `shouldSatisfy` isLeft
    it "multiple parens" $
      parseType "=> (a -> b) -> (b -> c) -> c -> d" `shouldBe`
      Right
        (ParsedType
           []
           (SignatureNode
              (SignatureNode
                 (SignatureLeaf (TypeVarId "a" :| []))
                 (SignatureLeaf (TypeVarId "b" :| [])))
              (SignatureNode
                 (SignatureNode
                    (SignatureLeaf (TypeVarId "b" :| []))
                    (SignatureLeaf (TypeVarId "c" :| [])))
                 (SignatureNode
                    (SignatureLeaf (TypeVarId "c" :| []))
                    (SignatureLeaf (TypeVarId "d" :| []))))))
