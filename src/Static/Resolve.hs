module Static.Resolve where

import           Data.Foldable   (toList)
import qualified Data.Map.Strict as M
import qualified Data.OrderedMap as OM
import qualified Data.Text       as T
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
