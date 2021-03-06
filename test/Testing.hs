{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Testing
  ( store
  , parseT
  , parseE
  ) where

import           Data.Parse   (parseString)
import           LExpr
import           LalaType     (LalaType, fromString)
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
  , ( "Head"
    , Item
        { typ = parseT "CSeq s, CMaybe m, Nil a => s a -> m a"
        , impl = JsLambda ["seq"] "seq[0]"
        })
  , ("Nil", Item {typ = parseT "CSeq s, Nil a => s a", impl = JsLambda [] "[]"})
  ]
