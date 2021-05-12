{-# LANGUAGE OverloadedLists #-}

module Data.DependencyGraphSpec
  ( spec
  ) where

import           Data.DependencyGraph
import           Test.Hspec

import           Data.Map             (Map, insert, (!), (!?))
import           Data.Set             (Set, elems)

-- |Count of all dependencies of a given id + 1.
-- |Sufficient to test traversal order.
dependencyCount :: Char -> [Int] -> Int
dependencyCount nodeId deps = 1 + sum deps

spec :: Spec
spec = do
  describe "traverseAccum" $ do
    it "empty" $
      traverseAccum dependencyCount (DependencyGraph []) `shouldBe` Right []
    it "singleton" $
      traverseAccum dependencyCount (DependencyGraph [('A', [])]) `shouldBe`
      Right [('A', 1)]
    it "single path" $
      traverseAccum
        dependencyCount
        (DependencyGraph [('A', ['B']), ('B', ['C']), ('C', [])]) `shouldBe`
      Right [('A', 3), ('B', 2), ('C', 1)]
  describe "topologicalOrder" $ do
    it "interconnected triple" $
      topologicalOrder
        (DependencyGraph [('A', ['B', 'C']), ('B', ['C']), ('C', [])]) `shouldBe`
      Right "ABC"
