{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module ResolvedExpression
  ( ResolvedExpression
  , ResolvedExpressionError(..)
  , resolvedExpression
  , procExpr
  , boundRefs
  , resolvedType
  , resolve
  , ResolveErr(..)
  , pieces
  ) where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified ProcessedExpression
import qualified Utils

import Data.Bifunctor (first)
import Data.Either (rights)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map, keys)
import Data.Set (Set, (\\), fromList)
import Impl
import PieceOfLogic
import ProcessedExpression
  ( ProcessedExpression
  , declaredType
  , expr
  , externalRefs
  )
import Type
import TypeCheck
  ( TypeCheckResolvedExpressionError(..)
  , typeCheckResolvedExpression
  )

type Ref = String

-- The result of processing an `Expression`.
-- Contains the original `Expression` and data inferred from it.
--
-- TODO: simplify resolution, get rid of `ProcessedExpression`, use the list monad
-- to model lookup ambiguity. Proxy piece lookup calls through `boundRefs`.
data ResolvedExpression =
  ResolvedExpression
    { procExpr :: ProcessedExpression
    , boundRefs :: Map Ref PieceOfLogic
    }
  deriving (Show, Eq)

data ResolvedExpressionError
  = UnboundRefs (Set String)
  | TypeCheckError TypeCheckResolvedExpressionError
  deriving (Eq, Show)

-- Validating constructor.
resolvedExpression ::
     ProcessedExpression
  -> Map Ref PieceOfLogic
  -> Either ResolvedExpressionError ResolvedExpression
resolvedExpression procExpr boundRefs = do
  checkUnboundRefs (externalRefs procExpr) (fromList . keys $ boundRefs)
  checkType boundRefs procExpr
  return $ ResolvedExpression procExpr boundRefs
  where
    checkUnboundRefs expected got =
      let missingKeys = expected \\ got
       in if Set.null missingKeys
            then Right ()
            else Left $ UnboundRefs missingKeys
    checkType lookup procExpr =
      first TypeCheckError (typeCheckResolvedExpression lookup procExpr)

-- | `ProcessedExpression` declares a type.
-- If it made it through the smart constructor, we can assume it is valid.
resolvedType = declaredType . procExpr

data ResolveErr
  = LookupFailed String
  | ResolvedExpressionError ResolvedExpressionError
  deriving (Eq, Show)

type Lookup = Ref -> [PieceOfLogic]

type NonEmptyLookup = Ref -> Either ResolveErr (NonEmpty PieceOfLogic)

-- Return all possible options of resolving an expression.
-- Explicitly fails if a ref was not found.
--
-- Refer to helper methods for details. In short, what happens in the implementation is:
-- 1. For each set of references, look each ref up. Each ref lookup may return multiple items.
-- 2. Combine all possible sets of resoultions by picking one `PieceOfLogic` for each reference.
-- 3. Repeat resolution for each bound `PieceOfLogic`.
--
-- Every step involiving lookup may return multiple results.
-- Some nested results may be incompatible (i.e. one ref bound to multiple items). Such results
-- are discarded.
--
-- TODO: handle cycles.
resolve ::
     Lookup -> ProcessedExpression -> Either ResolveErr [ResolvedExpression]
resolve lookup processedExpr = do
  resolvedBoundRefs <-
    resolveRefs
      (explicitlyFailingOnEmptyLookup lookup)
      (Set.elems $ ProcessedExpression.externalRefs processedExpr)
  sequence
    [ resolvedExpression' processedExpr boundRefs
    | boundRefs <- resolvedBoundRefs
    ]
  where
    explicitlyFailingOnEmptyLookup baseLookup k =
      case lookup k of
        [] -> Left $ LookupFailed k
        (x:xs) -> Right $ x :| xs
    resolvedExpression' pe br =
      Utils.mapLeft ResolvedExpressionError $ resolvedExpression pe br

-- Type alias to avoid having too many lists in signatures.
type DependencySet = [(Ref, PieceOfLogic)]

-- Resolving a single `Ref` may require binding multiple refs.
-- Those refs must be merged when building the final result.
type BoundRefs = Map Ref PieceOfLogic

-- Merge a set of bound references.
-- Rejects conflicting bindings.
mergeBoundRefs :: [BoundRefs] -> Either String BoundRefs
mergeBoundRefs =
  Utils.mapLeft ("Conflict: " ++) .
  Utils.fromListRejectOverlap . List.nub . concatMap Map.assocs

resolveRefs :: NonEmptyLookup -> [Ref] -> Either ResolveErr [BoundRefs]
resolveRefs lookup refs = do
  dependencySets <- lookupDependencySets lookup refs
  resolvedSets <- sequence $ resolveDependencySet lookup <$> dependencySets
  return $ concat . NonEmpty.toList $ resolvedSets

lookupDependencySets ::
     NonEmptyLookup -> [Ref] -> Either ResolveErr (NonEmpty DependencySet)
lookupDependencySets lookup refs = do
  implsPerRef :: [(Ref, NonEmpty PieceOfLogic)] <-
    traverse hoistSnd [(ref, lookup ref) | ref <- refs]
  return $ traverse sinkFst implsPerRef
  where
    hoistSnd :: (Applicative a) => (x, a y) -> a (x, y)
    hoistSnd (x, y) = (,) x <$> y
    sinkFst :: (Functor f) => (a, f b) -> f (a, b)
    sinkFst (a, bs) = fmap (a, ) bs

-- Resolve each dependency and combine valid options.
-- Returns all possible ways to resolve a DependencySet.
-- TODO: return a nested Either; Return NonEmpty and fail explicitly on empty results?
resolveDependencySet ::
     NonEmptyLookup -> DependencySet -> Either ResolveErr [BoundRefs]
resolveDependencySet lookup depSet = do
  resultPerItem :: [[BoundRefs]] <-
    sequence [resolveSingleItem lookup ref item | (ref, item) <- depSet]
  let resolutionVariants = sequence resultPerItem -- transposed results
  return $ rights $ mergeBoundRefs <$> resolutionVariants

resolveSingleItem ::
     NonEmptyLookup -> Ref -> PieceOfLogic -> Either ResolveErr [BoundRefs]
resolveSingleItem lookup ref externalItem =
  let addOwnBinding boundRefOptions =
        mappend (Map.singleton ref externalItem) <$> boundRefOptions
   in addOwnBinding <$> resolveSingleItem' lookup ref externalItem
  where
    resolveSingleItem' lookup ref (ImplPiece _) = Right [Map.empty]
    resolveSingleItem' lookup ref (ExprPiece processedExpression) =
      resolveRefs
        lookup
        (Set.elems $ ProcessedExpression.externalRefs processedExpression)

pieces :: ResolvedExpression -> [(String, PieceOfLogic)]
pieces (ResolvedExpression expr boundRefs) =
  Map.assocs boundRefs ++ [("Ret", ExprPiece expr)]
