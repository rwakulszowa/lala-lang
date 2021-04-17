{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module TypeCheck
  ( typeCheckExpression
  , TypeCheckError(..)
  , typeCheckPiece
  , TypeCheckPieceError(..)
  , typeCheckResolvedExpression
  , TypeCheckResolvedExpressionError(..)
  ) where

import Control.Monad (foldM, liftM)
import Data.Bifunctor (bimap, first)
import Data.BinaryTree (BinaryTree(..))
import Data.Bitraversable (bisequence)
import Data.Char (isLower)
import Data.Either (fromRight)
import Data.Map (Map, (!?))
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree

import LalaType (ApplyError, LalaType, MergeError)

import qualified Data.BiDiEqMap as BiDiEqMap
import qualified Data.DependencyGraph as DependencyGraph
import qualified Impl
import qualified LalaType
import qualified PieceOfLogic
import qualified ProcessedExpression
import qualified Utils

import Data.BiDiEqMap (BiDiEqMap)
import Data.DependencyGraph (DependencyGraph(..), DependencyGraphError(..))
import Expression
  ( Expression(..)
  , ImplExprLeaf(..)
  , ImplementationExpression(..)
  , Literal(..)
  , Ref(..)
  )
import Impl (Impl(..))
import PieceOfLogic (PieceOfLogic(..))
import ProcessedExpression (ProcessedExpression)
import Type (Type(..))
import Typiara.FT (FT(..))

-- | External references (e.g. injected `Impl`s) are looked up, rather than
-- resolved recursively.
type ExternalRefLookup = Map String LalaType

type RefLookupF = Ref -> Either TypeCheckError LalaType

-- | Build a lookup function from a map of external refs, extended with an
-- optional set of local refs.
makeLookupF :: [(String, LalaType)] -> ExternalRefLookup -> RefLookupF
makeLookupF localRefs externalRefLookup =
  let lookupMap =
        Map.fromList (first LocalRef <$> localRefs) <>
        Map.mapKeys ExternalRef externalRefLookup
   in (\ref -> maybe (Left $ RefLookupError ref) Right (lookupMap !? ref))

data TypeCheckError
  = RefLookupError Ref
  | MergeError
      { expr :: ImplementationExpression
      , err :: MergeError
      }
  | WrongArity
      { args :: [String]
      , tt :: LalaType
      }
  | ApplyError ApplyError
  deriving (Eq, Show)

-- | Validate that an expression is compatible with a declared type.
typeCheckExpression ::
     ExternalRefLookup -> LalaType -> Expression -> Either TypeCheckError ()
typeCheckExpression l t (ImplementationExpression impl) =
  typeCheckImplExpr (makeLookupF [] l) t impl
typeCheckExpression l t (FunctionExpression args impl) = do
  (argsWithTypes, remainingType) <-
    first (uncurry WrongArity) (popArgsFromType args t)
  typeCheckImplExpr (makeLookupF argsWithTypes l) remainingType impl

popArgsFromType ::
     [a] -> LalaType -> Either ([a], LalaType) ([(a, LalaType)], LalaType)
          -- ^ Zip each arg with an argument node from the TypeTree,
          -- unfolding the type in the process.
          -- `popArgsFromType (A -> B -> C) [a] = [(a, A)], (B -> C)
          -- Fails if the tree doesn't have enough arguments.
popArgsFromType [] t = Right ([], t)
popArgsFromType (a:as) t =
  case (LalaType.decompose t) of
    ("F", arg, ret) -> do
      (recArgs, recRet) <- popArgsFromType as ret
      return $ ((a, arg) : recArgs, recRet)
    otherwise -> Left ((a : as), t)

-- | Look up references, infer literals, reduce applications.
typeCheckImplExpr ::
     RefLookupF
  -> LalaType
  -> ImplementationExpression
  -> Either TypeCheckError ()
typeCheckImplExpr lookup typ expr =
  inferImplExpr lookup expr >>= typeCheckMerge expr typ

-- | Infer expression type.
-- To properly type check `ApplicationExpression`s, we must be able to know the
-- type of both the function and the argument. Simply comparing it with a
-- declared type won't do, as the declared type only defines the return value -
-- we would have no way of validating the argument and the actual application.
--
-- TLDR: `ImplementationExpression`s are inferred, everything above them is
-- type checked.
inferImplExpr ::
     RefLookupF -> ImplementationExpression -> Either TypeCheckError (LalaType)
inferImplExpr lookup (Leaf (ImplExprLit literal)) =
  Right (LalaType.singleton (inferLiteral literal))
inferImplExpr lookup (Leaf (ImplExprRef ref)) = lookup ref
inferImplExpr lookup (Node fun arg)
  -- Infer the result of applying `fun` to `arg`.
  -- Note, that we're returning the expression's return type, not the
  -- function's, i.e.
  -- the type of `2 + 2` is Int, not (Int -> Int -> Int)
 = do
  f <- inferImplExpr lookup fun
  a <- inferImplExpr lookup arg
  first ApplyError (LalaType.apply f a)

-- | Merge two types, failing if they're not compatible.
-- A relevant piece of expression is injected to build an error.
typeCheckMerge ::
     ImplementationExpression
  -> LalaType
  -> LalaType
  -> Either TypeCheckError ()
typeCheckMerge expr declared required =
  bimap (MergeError expr) (const ()) (declared `LalaType.merge` required)

-- | Literal types are inferred, rather than looked up.
inferLiteral (IntLiteral _) = T CNum
inferLiteral (StrLiteral _) = T CStr

-- | Type checking for `PieceOfLogic`.
-- TODO: move to a separate file.
data TypeCheckPieceError
  = TypeCheckExprError TypeCheckError
  | InvalidDeclaredType LalaType String
  deriving (Eq, Show)

-- |TypeCheck a type of a single, non-resolved `PieceOfLogic`, given a lookup
-- |of pieces with already known types.
-- |No recursion involved. All dependencies must be already resolved.
typeCheckPiece ::
     ExternalRefLookup -> PieceOfLogic -> Either TypeCheckPieceError ()
typeCheckPiece _ (ImplPiece (Impl typ _)) = Right ()
-- ^ Nothing to validate here.
-- Perhaps one day the system will be able to validate `Impl`s.
typeCheckPiece lookup (ExprPiece procExpr) = do
  let declaredTypeDef = ProcessedExpression.declaredType procExpr
  first TypeCheckExprError $
    typeCheckExpression
      lookup
      declaredTypeDef
      (ProcessedExpression.expr procExpr)

-- | Used to look up dependencies and types.
type PieceLookup = Map String PieceOfLogic

-- |A more specific way of identifying a root id that won't cause conflicts.
-- |Required, because the root in `typeCheckResolvedExpression` doesn't know its id.
-- |Using `Maybe` would provide the same functionality, but less obvious semantics.
-- |Using a sentinel string is error prone.
data RootOrNotRoot a
  = Root
  | NotRoot a
  deriving (Eq, Show, Ord)

unwrapRootOrNotRoot Root = Left RootUnwrap
unwrapRootOrNotRoot (NotRoot a) = Right a

data TypeCheckResolvedExpressionError
  = TypeCheckOnePieceError TypeCheckOnePieceError
  | DependencyGraphError (DependencyGraphError (RootOrNotRoot String))
  | DependencyCycle [String]
  | LookupFailure String
  | InvalidTypeDef (String)
  deriving (Eq, Show)

data DigDepsError
  = DigDepsLookupFailure String
  | DigDepsCycle [String]
  deriving (Eq, Show)

fromDigDepsError :: DigDepsError -> TypeCheckResolvedExpressionError
fromDigDepsError (DigDepsLookupFailure x) = LookupFailure x
fromDigDepsError (DigDepsCycle x) = DependencyCycle x

data TypeCheckOnePieceError
  = TypeCheckPieceError TypeCheckPieceError
  | RootUnwrap
  | PieceLookupFailure String
  deriving (Eq, Show)

-- |Type check a `ProcessedExpression`, along with its dependencies.
-- |Detects cyclic dependencies.
--
-- TODO: this function is more about traversal than about type checking.
-- Extract to a generic function.
typeCheckResolvedExpression ::
     PieceLookup
  -> ProcessedExpression
  -> Either TypeCheckResolvedExpressionError ()
typeCheckResolvedExpression l e =
  buildDependencyGraph l e >>= traverseTypeCheck l e
  where
    buildDependencyGraph ::
         PieceLookup
      -> ProcessedExpression
      -> Either TypeCheckResolvedExpressionError (DependencyGraph (RootOrNotRoot String))
    buildDependencyGraph l root = do
      let rootPiece = ExprPiece root
      -- Shake off unused dependencies.
      usedDeps <- Utils.mapLeft fromDigDepsError (digDeps l rootPiece)
      -- Turn the map into a graph of dependencies.
      return $
        DependencyGraph . Map.fromList $
        (Root, Set.map NotRoot (PieceOfLogic.pieceDeps rootPiece)) :
        [ (NotRoot id, Set.map NotRoot (PieceOfLogic.pieceDeps piece))
        | (id, piece) <- Map.assocs l
        , id `Set.member` usedDeps
        ]
      where
        digDeps ::
             PieceLookup -> PieceOfLogic -> Either DigDepsError (Set String)
        -- |Recursively return all dependencies.
        -- |Used to filter out items in the `lookup` that are not used by the expression.
        -- |Detects cyclic dependencies.
        digDeps = digDeps' mempty
          where
            digDeps' seen l p = do
              let ownDeps = PieceOfLogic.pieceDeps p
              childrenPieces <-
                mapM
                  (\dep ->
                     Utils.maybeError (DigDepsLookupFailure dep) $
                     (dep, ) <$> l !? dep)
                  (Set.elems ownDeps)
              dug <-
                mapM
                  (\(pieceId, piece) ->
                     if pieceId `elem` seen -- FIXME: O(n); a data structure for cycle detection (O(1) lookup + maintained order)
                                            -- / build the error on the way up
                       then Left $ DigDepsCycle (pieceId : seen)
                       else digDeps' (pieceId : seen) l piece)
                  childrenPieces
              return $ ownDeps `mappend` mconcat dug
    traverseTypeCheck ::
         PieceLookup
      -> ProcessedExpression
      -> DependencyGraph (RootOrNotRoot String)
      -> Either TypeCheckResolvedExpressionError ()
    -- | Traverse the dependencies, resolving them in topographic order.
    -- | Root passed in as an argument, because it won't be found in the regular lookup.
    traverseTypeCheck l r dg = do
      typeLookup <-
        first InvalidTypeDef . traverse (pure . PieceOfLogic.getType) $ l
      _ <-
        (Utils.mapLeft
           DependencyGraphError
           (DependencyGraph.traverseAccum (typeCheckPiece' typeLookup l r) dg) >>=
         Utils.mapLeft TypeCheckOnePieceError . sequence)
      return ()
      where
        typeCheckPiece' typeLookup nonRootLookup rootExpr pieceId recResults = do
          sequence recResults
          piece <- resolvePiece rootExpr pieceId nonRootLookup
          -- ^ Propagate child failures.
          first TypeCheckPieceError $ typeCheckPiece typeLookup piece
        resolvePiece rootExpr pieceId nonRootLookup =
          case pieceId of
            (NotRoot nonRootPieceId) ->
              Utils.maybeError (PieceLookupFailure nonRootPieceId) $
              nonRootLookup !? nonRootPieceId
            Root -> Right $ ExprPiece rootExpr
