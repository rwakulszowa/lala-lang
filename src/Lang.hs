module Lang
  ( Lang(..)
  , WithLang
  , getLang
  ) where

-- Supported external types.
-- `Lala` is not explicitly listed, because lala impls are represented
-- as `Expression`s.
data Lang
  = Py
  | Js
  deriving (Eq, Show, Ord)

class WithLang a where
  getLang :: a -> Lang
