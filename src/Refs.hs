module Refs
  ( Refs(..)
  ) where

import           Data.Map (Map)
import           LalaType (LalaType)

-- | Storage for all information related to available implementations.
-- Expected to be initialized once on startup and kept constant.
newtype RefData =
  RefData
    { typ :: LalaType
    }
  deriving (Eq, Show, Ord)

type Refs = Map String RefData
