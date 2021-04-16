module Static.Store where

import           Data.Map.Strict
import           LalaType
import           Static.Impl

-- | Store of implementations.
-- For now - one item per ref, to keep things trivial.
-- Will change once more languages are supported.
type Store = Map String Item

data Item =
  Item
    { impl :: Impl
    , typ  :: LalaType
    }
  deriving (Eq, Show, Ord)
