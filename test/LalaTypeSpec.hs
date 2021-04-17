{-# LANGUAGE OverloadedLists #-}

module LalaTypeSpec
  ( spec
  ) where

import LalaType
import Test.Hspec

import Data.Map (Map)
import Data.Tree (Tree(..))
import LalaType (LalaType(..))
import Parse
  ( ParsedConstraint(..)
  , ParsedType(..)
  , SignatureToken(..)
  , TypeTag(..)
  , TypeVarId(..)
  )
import Type (Type(..))
import Typiara.TypeDef (TypeDef(..))

t' :: Tree Char -> Map Char TypeTag -> LalaType
t' s c = either error id (lalaType s c)

spec :: Spec
spec = do
  describe "fromParsedType" $ do
    it "singleton" $
      fromParsedType
        (ParsedType
           [ParsedConstraint (TypeTag "Nil") (TypeVarId "a")]
           (SignatureLeaf (TypeVarId "a"))) `shouldBe`
      Right (t' (Node 'a' []) [('a', TypeTag "Nil")])
    it "trivial function" $
      fromParsedType
        (ParsedType
           [ ParsedConstraint (TypeTag "CNum") (TypeVarId "x")
           , ParsedConstraint (TypeTag "CStr") (TypeVarId "y")
           ]
           (SignatureNode
              (SignatureLeaf (TypeVarId "x"))
              (SignatureLeaf (TypeVarId "y")))) `shouldBe`
      Right
        (t'
           (Node 'a' [Node 'b' [], Node 'c' []])
           [('a', TypeTag "F"), ('b', TypeTag "CNum"), ('c', TypeTag "CStr")])
    it "linked function" $
      fromParsedType
        (ParsedType
           [ParsedConstraint (TypeTag "Nil") (TypeVarId "x")]
           (SignatureNode
              (SignatureLeaf (TypeVarId "x"))
              (SignatureLeaf (TypeVarId "x")))) `shouldBe`
      Right
        (t'
           (Node 'a' [Node 'b' [], Node 'b' []])
           [('a', TypeTag "F"), ('b', TypeTag "Nil")])
    it "nested with links" $
      fromParsedType
        (ParsedType
           [ ParsedConstraint (TypeTag "Nil") (TypeVarId "x")
           , ParsedConstraint (TypeTag "Nil") (TypeVarId "y")
           ]
           (SignatureNode
              (SignatureNode
                 (SignatureLeaf (TypeVarId "x"))
                 (SignatureLeaf (TypeVarId "y")))
              (SignatureLeaf (TypeVarId "x")))) `shouldBe`
      Right
        (t'
           (Node 'a' [Node 'b' [Node 'c' [], Node 'd' []], Node 'c' []])
           [ ('a', TypeTag "F")
           , ('b', TypeTag "F")
           , ('c', TypeTag "Nil")
           , ('d', TypeTag "Nil")
           ])
  describe "unLalaType" $ do
    it "singleton" $
      unLalaType (t' (Node 'a' []) [('a', TypeTag "CNum")]) `shouldBe`
      (Node (toEnum 0) [], [(toEnum 0, TypeTag "CNum")])
