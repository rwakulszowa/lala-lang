{-# LANGUAGE FlexibleInstances #-}

-- | This file is the main interface to the `Typiara` world.
-- Other modules should not import `Typiara` directly (with a few, minor
-- exceptions) - any required functionality should be imported from this
-- module.
module LalaType
  ( LalaType(..)
  , singleton
  , lalaType
  , unLalaType
  , fromParsedType
  , fromString
  , merge
  , MergeError
  , ApplyError
  , decompose
  , refresh
  , LalaType.apply
  ) where

import qualified Data.List                as List
import qualified Data.Map.Strict          as Map
import qualified Data.Set                 as Set
import qualified Data.Tree                as Tree
import qualified Typiara.TypeEnv          as TypeEnv

import           Control.Monad            ((>=>))
import           Data.List.NonEmpty       (NonEmpty (..))
import           Data.Traversable         (mapAccumL)
import           Text.Read                (readMaybe)

import           Data.Bifunctor           (first, second)
import           Data.Map                 (Map)
import           Data.Set                 (Set)
import           Data.Tree                (Tree (..))
import           Typiara                  (apply)
import           Typiara.Data.Tagged      (Tagged (..))
import           Typiara.FT               (FT (..))
import           Typiara.Infer.Expression (InferExpressionError)
import           Typiara.TypeEnv          (RootOrNotRoot (..), TypeEnv (..))

import           Parse                    (ParsedConstraint (..),
                                           ParsedType (..), SignatureToken (..),
                                           TypeTag (..), TypeVarId (..),
                                           parseType)
import           Type                     (Type (..))
import           Typiara.Utils            (allStrings, fromRight)
import           Utils                    (fromListRejectOverlap, replaceValues)

-- A result of processing a raw `ParsedType`.
-- Serves as an interface to the type checking world implemented by `Typiara`.
newtype LalaType =
  LalaType (TypeEnv Type Char)
  deriving (Eq, Ord)

instance Show LalaType where
  show (LalaType te) =
    let (shape, constraints) = TypeEnv.decompose te
     in "LalaType " ++
        wrapParens (showMap constraints ++ " => " ++ showTagTree shape)
    where
      wrapParens s = "(" ++ s ++ ")"
      showMap :: Map.Map Int String -> String
      showMap m =
        List.intercalate
          ", "
          [tag ++ " " ++ show var | (var, tag) <- Map.assocs m]
      showTagTree (Node x []) = show x
      showTagTree (Node x cs) =
        wrapParens (unwords (show x : (showTagTree <$> cs)))

un (LalaType x) = x

-- TODO: sane char, enumerating from 'a' to 'z'
type TypeIdent = Char

type ConstraintMap = Map TypeIdent TypeTag

-- Smart constructor.
lalaType :: Tree TypeIdent -> ConstraintMap -> Either String LalaType
lalaType shape constraints =
  LalaType <$>
  first show (TypeEnv.fromEnumTree shape (mapTypeTag <$> constraints))
  where
    mapTypeTag :: TypeTag -> String
          -- ^ unwrap a TypeTag and convert it to a `Tagged` representation.
          -- `Nil` and `F` are base variants. Custom types are prefixed with `T`.
    mapTypeTag (TypeTag "Nil") = "Nil"
    mapTypeTag (TypeTag "F")   = "F"
    mapTypeTag (TypeTag t)     = "T." ++ t

-- | Reverse of `lalaType`.
unLalaType :: LalaType -> (Tree TypeIdent, ConstraintMap)
unLalaType = second (fmap unpackTypeTag) . TypeEnv.decompose . un
  where
    unpackTypeTag :: String -> TypeTag
          -- ^ Dual to `mapTypeTag` above.
    unpackTypeTag ('T':'.':t) = TypeTag t
    unpackTypeTag t           = TypeTag t

singleton = LalaType . TypeEnv.singleton

-- `ParsedType` infers shape from the input string, but doesn't perform any
-- logic on the values.
-- This function enforces that the content is legit.
fromParsedType :: ParsedType TypeVarId -> Either String LalaType
fromParsedType = fromParsedType' . snd . replaceValues ['a' ..]

fromParsedType' :: ParsedType Char -> Either String LalaType
fromParsedType' (ParsedType constraints signature) = do
  let ((funIdents, externalIdentDiff), signatureShape) =
        refreshShapeIdents . signatureToShape $ signature
  refreshedConstraints <-
    mapM (mapConstraintThroughLookup externalIdentDiff) constraints
  externalConstraintMap <- buildConstraintMap refreshedConstraints
  lalaType
    signatureShape
    (externalConstraintMap <> buildImplicitFunConstraints funIdents)
  where
    mapConstraintThroughLookup typeIdentMapping (ParsedConstraint constraintId typeParam) =
      maybe
        (Left $ "typeParam not found: " ++ show typeParam)
        (Right . ParsedConstraint constraintId) $
      typeParam `Map.lookup` typeIdentMapping
    buildImplicitFunConstraints idents =
      Map.fromList [(ident, TypeTag "F") | ident <- idents]
    buildConstraintMap cs =
      first
        show
        (fromListRejectOverlap [(cT, cId) | (ParsedConstraint cId cT) <- cs])

fromString = parseType >=> fromParsedType

refresh = LalaType . TypeEnv.refreshTypeEnv . un

-- Each `ImplicitFunIdent` will be mapped to a new, unique `TypeIdent`. Not usable in `Map`s.
data ImplicitShapeIdent
  = ImplicitExternalIdent TypeIdent
  | ImplicitFunIdent
  deriving (Eq, Show)

-- Same as `ImplicitShapeIdent`, except each `ImplicitFunIdent` has been mapped to a unique id.
-- Can be used in a `Map`.
data ExplicitShapeIdent
  = ExplicitExternalIdent TypeIdent
  | ExplicitFunIdent Int
  deriving (Eq, Show, Ord)

-- Builds a `Tree` instance, filling `Node`s with sentinel fresh idents that
-- should be added to the `ConstraintMap` with a function constraint.
signatureToShape :: SignatureToken TypeIdent -> Tree ImplicitShapeIdent
signatureToShape (SignatureLeaf (rootIdent :| args)) =
  Node
    (ImplicitExternalIdent rootIdent)
    [Node (ImplicitExternalIdent a) [] | a <- args]
signatureToShape (SignatureNode left right) =
  Node ImplicitFunIdent (map signatureToShape [left, right])

-- Map external and implicit idents to a fresh set of `TypeIdent`s.
refreshShapeIdents ::
     Tree ImplicitShapeIdent
  -> (([TypeIdent], Map TypeIdent TypeIdent), Tree TypeIdent)
refreshShapeIdents =
  first partitionExplicitShapeIdentMap .
  explicitTreeToTypeIdentTree . implicitTreeToExplicitTree
  where
    implicitTreeToExplicitTree = snd . mapAccumL implicitToExplicit [0 ..]
      where
        implicitToExplicit identSource (ImplicitExternalIdent ident) =
          (identSource, ExplicitExternalIdent ident)
        implicitToExplicit (newIdent:identSource) ImplicitFunIdent =
          (identSource, ExplicitFunIdent newIdent)
    explicitTreeToTypeIdentTree = replaceValues ['a' ..]
          -- returns (addedFunIdents, externalIdentsMapping)
    partitionExplicitShapeIdentMap ::
         Map ExplicitShapeIdent TypeIdent
      -> ([TypeIdent], Map TypeIdent TypeIdent)
    partitionExplicitShapeIdentMap = Map.foldrWithKey pickDst (mempty, mempty)
      where
        pickDst k v (funIdents, externalIdentMapping) =
          case k of
            (ExplicitExternalIdent ident) ->
              (funIdents, Map.insert ident v externalIdentMapping)
            (ExplicitFunIdent i) -> (v : funIdents, externalIdentMapping)

newtype MergeError =
  MergeError (TypeEnv.UnifyEnvError Char)
  deriving (Eq, Show)

merge :: LalaType -> LalaType -> Either MergeError LalaType
merge (LalaType x) (LalaType y) =
  LalaType <$> first MergeError (TypeEnv.unifyEnv Root x y)

newtype ApplyError =
  ApplyError (InferExpressionError Char)
  deriving (Eq, Show)

apply :: LalaType -> LalaType -> Either ApplyError LalaType
apply (LalaType f) (LalaType x) =
  LalaType <$> first ApplyError (Typiara.apply f [x])

-- TODO: use `TypeEnv.popArg`.
decompose :: LalaType -> (String, LalaType, LalaType)
decompose (LalaType t) =
  let r = TypeEnv.getRoot t
   in case r of
        (F a b) -> (tag r, LalaType (pick' a), LalaType (pick' b))
  where
    pick' = TypeEnv.pick t . NotRoot
