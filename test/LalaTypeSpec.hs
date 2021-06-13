{-# LANGUAGE OverloadedLists #-}

module LalaTypeSpec
  ( spec
  ) where

import           Test.Hspec

import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Map           (Map)
import           Data.Tree          (Tree (..))
import           LalaType
import           ParsedType         (ParsedConstraint (..), ParsedType (..),
                                     SignatureToken (..), TypeTag (..),
                                     TypeVarId (..))
import           Testing            (parseT)
import           Type               (Type (..))

t' :: Tree Char -> Map Char TypeTag -> LalaType
t' s c = either error id (lalaType s c)

leaf = (`Node` [])

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
      "LalaType (T.CNum 1 => (1 -> 1))"
    it "nested" $
      show
        (t'
           (Node 'a' [Node 'b' [Node 'c' []], Node 'd' []])
           [ ('a', TypeTag "F")
           , ('b', TypeTag "CSeq")
           , ('c', TypeTag "Nil")
           , ('d', TypeTag "CNum")
           ]) `shouldBe`
      "LalaType (T.CSeq 1, Nil 2, T.CNum 3 => ((1 2) -> 3))"
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
    it "many instances of one generic" $
      fromParsedType
        (ParsedType
           [ ParsedConstraint (TypeTag "CSeq") (TypeVarId "s")
           , ParsedConstraint (TypeTag "CNum") (TypeVarId "a")
           , ParsedConstraint (TypeTag "CStr") (TypeVarId "b")
           ]
           (SignatureNode
              (SignatureLeaf (TypeVarId "s" :| [TypeVarId "a"]))
              (SignatureLeaf (TypeVarId "s" :| [TypeVarId "b"])))) `shouldBe`
      Right
        (t'
           (Node 'a' [Node 'b' [leaf 'c'], Node 'd' [leaf 'e']])
           [ ('a', TypeTag "F")
           , ('b', TypeTag "CSeq")
           , ('c', TypeTag "CNum")
           , ('d', TypeTag "CSeq")
           , ('e', TypeTag "CStr")
           ])
  describe "unLalaType" $ do
    it "singleton" $
      unLalaType (t' (Node 'a' []) [('a', TypeTag "CNum")]) `shouldBe`
      (Node (toEnum 0) [], [(toEnum 0, TypeTag "CNum")])
  describe "unParse" $ do
    it "nested" $
      unParse
        (t'
           (Node 'a' [Node 'b' [], Node 'c' [Node 'd' []]])
           [ ('a', TypeTag "F")
           , ('b', TypeTag "CNum")
           , ('c', TypeTag "CSeq")
           , ('d', TypeTag "CNum")
           ]) `shouldBe`
      "CNum B, CSeq C, CNum D => B -> C D"
  describe "applyAt" $ do
    let applyAt' x i f = applyAt f x i
    it "(a -> b -> a) | _ Num | Str" $ do
      let tn = singletonT CNum
      let ts = singletonT CStr
      let tf = makeFun [[0, 1, 0]]
      (pure tf >>= applyAt' tn 1 >>= applyAt' ts 0) `shouldBe` Right ts
  describe "reorderT" $ do
    it "Foldl_120" $
      -- Validate that reordering doesn't lose any information.
      -- The chosen function and order are non trivial.
     do
      let tFoldl =
            parseT "CSeq s, Nil a, Nil b => (a -> b -> a) -> a -> s b -> a"
      let (Just tRe) = reorderT [1, 2, 0]
      tRe `apply` tFoldl `shouldBe`
        Right (parseT "CSeq s, Nil a, Nil b => a -> s b -> (a -> b -> a) -> a")
