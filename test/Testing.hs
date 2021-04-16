{-# LANGUAGE OverloadedLists #-}

module Testing
  ( store
  , parseT
  , parseE
  ) where

import           Data.Parse   (parseString)
import           LalaType     (LalaType, fromString)
import           LExpr
import           Static.Impl
import           Static.Store
import           Value

-- | Parse type or crash.
parseT :: String -> LalaType
parseT s = either (error s) id (fromString s)

-- | Parse LExpr or crash.
parseE :: String -> LExpr Value
parseE s = either (error s) id (parseString s)

-- | Store with a few valid items.
-- Should suffice for tests.
store :: Store
store =
  [ ( "Add"
    , Item
        { typ = parseT "CNum a => a -> a -> a"
        , impl = JsLambda ["x", "y"] "x + y"
        })
  , ( "Inc"
    , Item
        { typ = parseT "CNum a => a -> a"
        , impl = LalaImpl ["x"] (ref "Add" |< intLiteral 1 |< ref "x")
        })
  , ( "Show"
    , Item
        { typ = parseT "CNum a, CStr b => a -> b"
        , impl = JsLambda ["x"] "x.toString()"
        })
  ]
