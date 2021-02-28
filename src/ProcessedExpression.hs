module ProcessedExpression
  ( ProcessedExpression
  , expr
  , externalRefs
  , declaredType
  , fromExpression
  ) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Utils

import Data.Either (rights)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Set (Set)
import Data.Tuple (swap)
import Expression
import Impl
import LalaType (LalaType)

-- The result of processing an `Expression`.
-- Contains the original `Expression` and data inferred from it.
data ProcessedExpression =
  ProcessedExpression
    { expr :: Expression
    , externalRefs :: Set String
    , declaredType :: LalaType
    }
  deriving (Show, Eq)

-- Public constructor. `data` constructor is hidden to disallow building out of sync instances.
fromExpression :: Expression -> LalaType -> ProcessedExpression
fromExpression e t =
  ProcessedExpression e (Set.fromList . digExternalRefs $ e) t
