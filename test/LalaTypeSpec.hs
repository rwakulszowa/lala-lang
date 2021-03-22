{-# LANGUAGE OverloadedLists #-}

module LalaTypeSpec
  ( spec
  ) where

import LalaType
import Test.Hspec

import Data.List.NonEmpty (NonEmpty(..))
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

t' :: Tree Char -> Map Char TypeTag -> LalaType
t' s c = either error id (lalaType s c)

spec :: Spec
spec = do
  describe "show" $ do
    it "singleton" $
      show (t' (Node 'a' []) [('a', TypeTag "CNum")]) `shouldBe`
      "LalaType (T.CNum 0 => 0)"
    it "linked function" $
      show
        (t'
           (Node 'a' [Node 'b' [], Node 'b' []])
           [('a', TypeTag "F"), ('b', TypeTag "CNum")]) `shouldBe`
      "LalaType (F 0, T.CNum 1 => (0 1 1))"
    it "nested" $
      show
        (t'
           (Node 'a' [Node 'b' [Node 'c' []], Node 'd' []])
           [ ('a', TypeTag "F")
           , ('b', TypeTag "CSeq")
           , ('c', TypeTag "Nil")
           , ('d', TypeTag "CNum")
           ]) `shouldBe`
      "LalaType (F 0, T.CSeq 1, Nil 2, T.CNum 3 => (0 (1 2) 3))"
  describe "fromParsedType" $ do
    it "singleton" $
      fromParsedType
        (ParsedType
           [ParsedConstraint (TypeTag "Nil") (TypeVarId "a")]
           (SignatureLeaf (TypeVarId "a" :| []))) `shouldBe`
      Right (t' (Node 'a' []) [('a', TypeTag "Nil")])
    it "trivial function" $
      fromParsedType
        (ParsedType
           [ ParsedConstraint (TypeTag "CNum") (TypeVarId "x")
           , ParsedConstraint (TypeTag "CStr") (TypeVarId "y")
           ]
           (SignatureNode
              (SignatureLeaf (TypeVarId "x" :| []))
              (SignatureLeaf (TypeVarId "y" :| [])))) `shouldBe`
      Right
        (t'
           (Node 'a' [Node 'b' [], Node 'c' []])
           [('a', TypeTag "F"), ('b', TypeTag "CNum"), ('c', TypeTag "CStr")])
    it "linked function" $
      fromParsedType
        (ParsedType
           [ParsedConstraint (TypeTag "Nil") (TypeVarId "x")]
           (SignatureNode
              (SignatureLeaf (TypeVarId "x" :| []))
              (SignatureLeaf (TypeVarId "x" :| [])))) `shouldBe`
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
                 (SignatureLeaf (TypeVarId "x" :| []))
                 (SignatureLeaf (TypeVarId "y" :| [])))
              (SignatureLeaf (TypeVarId "x" :| [])))) `shouldBe`
      Right
        (t'
           (Node 'a' [Node 'b' [Node 'c' [], Node 'd' []], Node 'c' []])
           [ ('a', TypeTag "F")
           , ('b', TypeTag "F")
           , ('c', TypeTag "Nil")
           , ('d', TypeTag "Nil")
           ])
    it "type application" $
      fromParsedType
        (ParsedType
           [ ParsedConstraint (TypeTag "CSeq") (TypeVarId "s")
           , ParsedConstraint (TypeTag "CNum") (TypeVarId "a")
           ]
           (SignatureLeaf (TypeVarId "s" :| [TypeVarId "a"]))) `shouldBe`
      Right
        (t'
           (Node 'a' [Node 'b' []])
           [('a', TypeTag "CSeq"), ('b', TypeTag "CNum")])
  describe "unLalaType" $ do
    it "singleton" $
      unLalaType (t' (Node 'a' []) [('a', TypeTag "CNum")]) `shouldBe`
      (Node (toEnum 0) [], [(toEnum 0, TypeTag "CNum")])
