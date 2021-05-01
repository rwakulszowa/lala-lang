{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}

-- | This file is the main interface to the `Typiara` world.
-- Other modules should not import `Typiara` directly (with a few, minor
-- exceptions) - any required functionality should be imported from this
-- module.
module LalaType
  ( LalaType(..)
  , singleton
  , singletonT
  , empty
  , lalaType
  , unLalaType
  , unParse
  , fromParsedType
  , fromString
  , arity
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
import           Data.Parse
import           Data.Set                 (Set)
import           Data.Tree                (Tree (..))
import           ParsedType
import           Type                     (Type (..))
import           Typiara                  (apply)
import           Typiara.Data.Tagged      (Tagged (..))
import           Typiara.FT               (FT (..))
import           Typiara.Infer.Expression (InferExpressionError)
import           Typiara.TypeEnv          (RootOrNotRoot (..), TypeEnv (..))
import           Typiara.Utils            (allStrings, fromRight)
import           Utils                    (fromListRejectOverlap, replaceValues,
                                           unionMapsRejectOverlap)

-- A result of processing a raw `ParsedType`.
-- Serves as an interface to the type checking world implemented by `Typiara`.
newtype LalaType =
  LalaType
    { typeEnv :: TypeEnv Type Char
    }
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

-- | Exported for convenience - allows one to avoid importing `T` explicitly.
singletonT = singleton . T

empty = LalaType (TypeEnv.singleton Nil)

-- `ParsedType` infers shape from the input string, but doesn't perform any
-- logic on the values.
-- This function enforces that the content is legit.
-- TODO: consider moving helpers to a separate file / ParsedType. This file should contain the interface, not the details.
fromParsedType :: ParsedType TypeVarId -> Either String LalaType
fromParsedType = fromParsedType' . snd . replaceValues ['a' ..]

fromParsedType' :: ParsedType Char -> Either String LalaType
fromParsedType' (ParsedType constraints signature) = do
  let ((funs, gens, grounds), signatureShape) =
        refreshShapeIdents . signatureToShape $ signature
  -- Each generic may map to multiple new ids.
  -- Each ground maps to a single new id.
  -- Combine all into a single diff from old ids into fresh ones.
  extendedDiff <- first show (unionMapsRejectOverlap [gens, (: []) <$> grounds])
  refreshedConstraints <-
    extendConstraints extendedDiff >>= first show . fromListRejectOverlap
  lalaType signatureShape (refreshedConstraints <> funConstraints funs)
  where
    applyMap m x = maybe (Left ("Key not found: " ++ show x)) Right (m Map.!? x)
    funConstraints idents =
      Map.fromList [(ident, TypeTag "F") | ident <- idents]
    -- | A list representation of constraints.
    -- Added here for readability - variable naming and types are a bit confusing.
    -- Remove after cleaning up `ParsedType`.
    constraintsList :: [(Char, TypeTag)]
    constraintsList = [(cT, cId) | (ParsedConstraint cId cT) <- constraints]
    -- | The amount of unique constraints may grow if generics are used.
    -- Convert the original items into a list with potential new items, according to a diff `m`.
    extendConstraints :: Map Char [Char] -> Either String [(Char, TypeTag)]
    extendConstraints m = concat <$> traverse f constraintsList
      where
        f (a, b) = fmap (, b) <$> applyMap m a

-- | Convert back to a string representation.
-- `F` constraints are excluded from the string, as they're implicitly defined by "->".
intoParsedType :: LalaType -> ParsedType TypeVarId
intoParsedType lt =
  let (signature, constraints) = unLalaType lt
   in ParsedType
        (intoParsedConstraint <$>
         (filter ((/= fTag) . snd) . Map.assocs) constraints)
        (intoSignatureToken constraints signature)
  where
    intoParsedConstraint (v, tag) =
      ParsedConstraint tag (TypeVarId (readableId v))
    intoSignatureToken cs (Node v [a, b])
      | cs Map.! v == fTag =
        SignatureNode (intoSignatureToken cs a) (intoSignatureToken cs b)
    -- Nested non-function tokes are currently not supported by the parser.
    -- Things like `s (s a)`.
    intoSignatureToken _ (Node v vs) =
      SignatureLeaf (TypeVarId . readableId <$> v :| (rootLabel <$> vs))
    fTag = TypeTag "F"
    -- Convert an enum into a readable char.
    -- TODO: just use ints
    readableId = cons . toEnum . (+ 65) . fromEnum
    cons x = [x]

fromString = parseString >=> fromParsedType

unParse = unParseType . intoParsedType

refresh = LalaType . TypeEnv.refreshTypeEnv . un

-- | Idents provided by the user, enriched with implicit data.
data ImplicitShapeIdent
  -- | Ground (right-most) type ident. Those are the only idents that can be
  -- directly linked by the user.
  = IGround TypeIdent
  -- | Generic (non right-most) type ident. Each instantiation will be replaced by a unique explicit ident.
  -- Generics should not be directly linked by the user; they may become linked by applying linked ground types.
  -- The original ident is stored for lookup in the constraint map.
  | IGeneric TypeIdent
  -- | Function (* -> *) node. Cannot be linked in any circumstances. Will be replaced by unique idents.
  | IFun
  deriving (Eq, Show)

-- | `ImplicitShapeIdent` decorated with freshly generated values, where applicable.
data ExplicitShapeIdent
  = EGround TypeIdent
  | EGeneric Int TypeIdent
  | EFun Int
  deriving (Eq, Show, Ord)

-- | Convert a single token to a tree.
-- Leaves do not handle nested application - the first item is the root, all remaining items are its params.
signatureToShape :: SignatureToken TypeIdent -> Tree ImplicitShapeIdent
signatureToShape (SignatureLeaf (ident :| [])) = Node (IGround ident) []
signatureToShape (SignatureLeaf (root :| args)) =
  Node (IGeneric root) [Node (IGround a) [] | a <- args]
signatureToShape (SignatureNode left right) =
  Node IFun (map signatureToShape [left, right])

type AddedFunIdents = [TypeIdent]

type MappedGenericIdents = Map TypeIdent [TypeIdent]

type MappedGroundIdents = Map TypeIdent TypeIdent

-- | Map external and implicit idents to a fresh set of `TypeIdent`s.
-- Returns the new tree and all transitions from old to new ids.
refreshShapeIdents ::
     Tree ImplicitShapeIdent
  -> ((AddedFunIdents, MappedGenericIdents, MappedGroundIdents), Tree TypeIdent)
refreshShapeIdents =
  first partitionExplicitShapeIdentMap .
  explicitTreeToTypeIdentTree . implicitTreeToExplicitTree
  where
    implicitTreeToExplicitTree = snd . mapAccumL iToE [0 ..]
      where
        iToE is (IGround ident)      = (is, EGround ident)
        iToE (i:is) (IGeneric ident) = (is, EGeneric i ident)
        iToE (i:is) IFun             = (is, EFun i)
    explicitTreeToTypeIdentTree = replaceValues ['a' ..]
    partitionExplicitShapeIdentMap =
      Map.foldrWithKey pickDst (mempty, mempty, mempty)
      where
        pickDst k v (funs, gens, grounds) =
          case k of
            (EGround ident) -> (funs, gens, Map.insert ident v grounds)
            (EGeneric i ident) ->
              (funs, Map.insertWith mappend ident [v] gens, grounds)
            -- ^ TODO: replace `insertWith` with dedicated `alter` for maps of lists. Prefer consing over concatenating.
            (EFun i) -> (v : funs, gens, grounds)

arity :: LalaType -> Int
arity = TypeEnv.arity . typeEnv

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
