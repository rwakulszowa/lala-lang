{-# LANGUAGE TupleSections #-}

module Data.DependencyGraph
  ( DependencyGraph(..)
  , DependencyGraphError(..)
  , traverseAccum
  ) where

import Control.Monad ((>>=))
import Data.Foldable (foldrM)
import Data.Graph (Graph)
import Data.Map (Map, (!?))
import Data.Maybe (fromJust)
import Data.Set (Set)

import qualified Data.Graph as Graph
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Utils

-- |Edges of elements with dependencies.
-- |Provides a way to traverse the items in topological order.
newtype DependencyGraph a =
  DependencyGraph
    { edges :: Map a (Set a)
    }
  deriving (Eq, Show)

data DependencyGraphError a
  = Cycle [a]
  | LookupError a
  | ProcessingOrderError a
  deriving (Eq, Show)

-- |Provide a sequence of ids that satisfy the ordering property, namely,
-- |that leaves are processed before nodes.
-- |TODO: detect cycles
topologicalOrder ::
     (Ord a) => DependencyGraph a -> Either (DependencyGraphError a) [a]
topologicalOrder (DependencyGraph edges) =
  let (graph, nodeFromVertex, vertexFromKey) =
        Graph.graphFromEdges
          [ ((), node, Set.elems children)
          | (node, children) <- Map.assocs edges
          ]
   in Right $ pickId . nodeFromVertex <$> Graph.topSort graph
  where
    pickId (_, id, _) = id

-- |Traverse items in topological order, accumulating a result in the process.
-- |For each item, processes its dependencies first.
-- |Each item is processed only once, and its results memoized.
traverseAccum ::
     (Ord a)
  => (a -> [b] -> b)
  -> DependencyGraph a
  -> Either (DependencyGraphError a) (Map a b)
traverseAccum f dg =
  topologicalOrder dg >>= traverse (withDeps dg) >>=
  foldrM (flip (adaptToState f)) mempty
  where
    withDeps dg a = Utils.maybeError (LookupError a) $ (a, ) <$> edges dg !? a
    adaptToState f state (a, deps)
      -- Results are only comuted once per id.
      -- If an item has been processed before, skip it.
      | a `Map.member` state = Right state
      | otherwise = do
        resolvedDeps <- traverse (lookupDep state) (Set.elems deps)
        let result = f a resolvedDeps
        return $ Map.insert a result state
      where
        lookupDep state id =
          Utils.maybeError (ProcessingOrderError id) $ state !? id
