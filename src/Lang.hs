{-# LANGUAGE TemplateHaskell #-}

module Lang
  ( Lang(..)
  , WithLang
  , getLang
  ) where

import Data.Aeson.TH (allNullaryToStringTag, defaultOptions, deriveJSON)

-- Supported external types.
-- `Lala` is not explicitly listed, because lala impls are represented
-- as `Expression`s.
data Lang
  = Py
  | Js
  deriving (Eq, Show, Ord)

$(deriveJSON defaultOptions {allNullaryToStringTag = False} ''Lang)

class WithLang a where
  getLang :: a -> Lang
