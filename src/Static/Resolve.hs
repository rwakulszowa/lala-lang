module Static.Resolve where

import           Data.Bifunctor       (first)
import qualified Data.DependencyGraph as DG
import           Data.Foldable        (toList)
import qualified Data.Map.Strict      as M
import           Data.Maybe
import qualified Data.OrderedMap      as OM
import qualified Data.Text            as T
import           Lang
import           LExpr
import           Static.Impl
import           Static.Store
import           Value

data Binding =
  Binding
    { key  :: T.Text
    , item :: Item
    }
  deriving (Eq, Show, Ord)

-- | An expression with all external references bound to specific items.
-- `bindings` is a list to maintain ordering.
data Resolved =
  Resolved
    { expr     :: LExpr Value
    , bindings :: [Binding]
    }
  deriving (Eq, Show)

newtype ResolveError =
  RefNotFound T.Text
  deriving (Eq, Show, Ord)

-- | Bind each deendency with an item from `Store`.
-- NOTE: heavily depends on the fact that only one item is stored per key in `Store`.
-- The approach will have to be updated if that no longer holds:
--  - the return value should become a list with multiple resolution variants
--  - recursion should be done in a sequential manner, instead of a tree-like one. Bindings found in the left branch
--    *do* affect resolution options in the right branch, which in turn affects further dependency lookup.
resolve :: Store -> LExpr Value -> Either ResolveError Resolved
resolve s le = do
  bindings <- traverse (go OM.omnull) (toList $ refs le)
  return $
    Resolved
      le
      (fmap (uncurry Binding) . OM.omassocs . OM.rightBiasedConcat $ bindings)
  where
    go bindings ref
      | bindings `OM.ommember` ref = Right bindings
    -- ^ Short circuit to break cycles.
    go bindings ref = do
      item <- maybe (Left $ RefNotFound ref) Right (s M.!? ref)
      let bindings' = OM.ominsertOrDrop ref item bindings
      case (toList . directDeps . impl) item
        -- No more dependencies - return accumulated items.
            of
        []  -> Right bindings'
        -- Handle children.
        dug -> OM.rightBiasedConcat <$> traverse (go bindings') dug

-- | Store, where all items have been converted to `lang`.
-- Items incompatible with `lang` are dropped.
-- After resoltion, there will be at most one item per reference.
--
-- NOTE: even though it is not used anywhere (yet?), it is possible to define
-- multiple items per reference, in the same language. The consequence is that
-- there may exist multiple ways to resolve each set of items which, in turn, means
-- there are many `ResolvedStore`s per `Store`.
-- Either support such scenarios or reject them during construction.
--
-- TODO: remove the other resolution method. It requires too much repetitive work.
data ResolvedStore =
  ResolvedStore
    { rlang     :: Lang
    , rbindings :: [Binding]
    }
  deriving (Eq, Show, Ord)

resolveStore :: Store -> Lang -> Either String ResolvedStore
resolveStore s l =
  ResolvedStore l . fmap bind <$>
  first show (reverse <$> DG.topologicalOrder depGraph)
  where
    substore = M.filter matchesLang s
      where
        matchesLang item = lang (impl item) `elem` [l, Lala]
    depGraph = DG.DependencyGraph (directItemDeps <$> substore)
      where
        directItemDeps = directDeps . impl
    bind k = Binding {key = k, item = substore M.! k}

-- | Turn a ResolvedStore into a source file.
-- Transpilation is never expected to fail - incompatible items are expected to have been filtered out during
-- construction
-- (TODO: hide the default constructor)
rStoreToSrc :: ResolvedStore -> T.Text
rStoreToSrc (ResolvedStore rlang rbindings) = T.unlines (bindb <$> rbindings)
  where
    bindb (Binding id item) =
      fromJust $ bind Js id (fromJust $ tosrc rlang (impl item))
