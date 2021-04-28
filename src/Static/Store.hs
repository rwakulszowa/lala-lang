module Static.Store where

import           Data.Map.Strict
import qualified Data.Text       as T
import           LalaType
import           Static.Impl

-- | Store of implementations.
-- For now - one item per ref, to keep things trivial.
-- Will change once more languages are supported.
type Store = Map T.Text Item

data Item =
  Item
    { impl :: Impl
    , typ  :: LalaType
    }
  deriving (Eq, Show, Ord)
