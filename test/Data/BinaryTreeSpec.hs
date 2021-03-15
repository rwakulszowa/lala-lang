module Data.BinaryTreeSpec
  ( spec
  ) where

import Data.BinaryTree
import Test.Hspec

spec :: Spec
spec = do
  describe "preOrder" $ do
    it "simple" $ do
      let t = Node (Leaf 'a') (Leaf 'b')
      preOrder t `shouldBe` [Nothing, Just 'a', Just 'b']
    it "nested" $ do
      let t = Node (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))) (Leaf 'd')
      preOrder t `shouldBe`
        [Nothing, Nothing, Just 'a', Nothing, Just 'b', Just 'c', Just 'd']
  describe "unPreOrder" $ do
    it "simple" $ do
      let l = [Nothing, Just 'a', Just 'b']
      unPreOrder l `shouldBe` (Node (Leaf 'a') (Leaf 'b'))
    it "nested" $ do
      let l =
            [Nothing, Nothing, Just 'a', Nothing, Just 'b', Just 'c', Just 'd']
      unPreOrder l `shouldBe`
        (Node (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))) (Leaf 'd'))
