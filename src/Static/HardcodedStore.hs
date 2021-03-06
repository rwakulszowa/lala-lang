{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A hardcoded set of implementations added for convenience.
-- In practice, store should be loaded dynamically.
module Static.HardcodedStore
  ( store
  ) where

import           Data.Parse
import           LExpr
import           LalaType
import           Static.Impl
import           Static.Store
import           Value

t :: String -> LalaType
t s = rightOrErr s (fromString s)

e :: String -> LExpr Value
e s = rightOrErr s (parseString s)

rightOrErr ctx (Left e) = error . show $ ("Parse failed: " ++ e, ctx)
rightOrErr _ (Right x)  = x

store :: Store
store = jsItems <> lalaItems
  where
    jsItems
      -- Numbers
     =
      [ ( "Add"
        , Item
            { typ = t "CNum a => a -> a -> a"
            , impl = JsLambda ["x", "y"] "x + y"
            })
      , ( "Inverse"
        , Item {typ = t "CNum a => a -> a", impl = JsLambda ["x"] "-x"})
      , ( "Mul"
        , Item
            { typ = t "CNum a => a -> a -> a"
            , impl = JsLambda ["x", "y"] "x * y"
            })
      -- Strings
      , ( "Show"
        , Item
            { typ = t "Nil a, CStr s => a -> s"
            , impl = JsLambda ["x"] "x.toString()"
            })
      , ( "Lower"
        , Item
            { typ = t "CStr a => a -> a"
            , impl = JsLambda ["x"] "x.toLowerCase()"
            })
      , ( "Upper"
        , Item
            { typ = t "CStr a => a -> a"
            , impl = JsLambda ["x"] "x.toUpperCase()"
            })
      , ( "Join"
        , Item
            { typ = t "CSeq s, CStr a => a -> s a -> a"
            , impl = JsLambda ["sep", "seq"] "seq.join(sep)"
            })
      , ( "Split"
        , Item
            { typ = t "CSeq s, CStr a => a -> a -> s a"
            , impl = JsLambda ["x", "sep"] "x.split(sep)"
            })
      -- Sequences
      , ( "Head"
        , Item
            { typ = t "CSeq s, CMaybe m, Nil a => s a -> m a"
            , impl = JsLambda ["seq"] "seq.length > 0 ? seq[0] : null"
            })
      , ( "Tail"
        , Item
            { typ = t "CSeq s, Nil a => s a -> s a"
            , impl = JsLambda ["seq"] "seq.slice(1)"
            })
      , ( "Cons"
        , Item
            { typ = t "CSeq s, Nil a => a -> s a -> s a"
            , impl = JsLambda ["head", "tail"] "[head].concat(tail)"
            })
      , ("Nil", Item {typ = t "CSeq s, Nil a => s a", impl = JsLambda [] "[]"})
      , ( "Map"
        , Item
            { typ = t "CSeq s, Nil a, Nil b => (a -> b) -> s a -> s a"
            , impl = JsLambda ["f", "seq"] "seq.map(f)"
            })
      , ( "Filter"
        , Item
            { typ = t "CSeq s, Nil a, CBool b => (a -> b) -> s a -> s a"
            , impl = JsLambda ["f", "seq"] "seq.filter(f)"
            })
      , ( "Foldl"
        , Item
            { typ = t "CSeq s, Nil a, Nil b => (a -> b -> a) -> a -> s b -> a"
            , impl =
                JsLambda
                  ["f", "zero", "seq"]
                  "seq.reduce((acc, el) => f(acc)(el), zero)"
            })
      -- Maybe
      , ( "Fmap"
        , Item
            { typ = t "CMaybe m, Nil a, Nil b => (a -> b) -> m a -> m b"
            , impl = JsLambda ["f", "m"] "m && f(m)"
            })
      , ( "Combine"
        , Item
            { typ = t "CMaybe m, Nil a, Nil b => m a -> (a -> m b) -> m b"
            , impl = JsLambda ["m", "f"] "m && f(m)"
            })
      -- Pairs
      , ( "Pair"
        , Item
            { typ = t "Nil a, Nil b, CProd p => a -> b -> p a b"
            , impl = JsLambda ["a", "b"] "[a, b]"
            })
      , ( "Swap"
        , Item
            { typ = t "Nil a, Nil b, CProd p => p a b -> p b a"
            , impl = JsLambda ["x"] "[x[1], x[0]]"
            })
      , ( "Fst"
        , Item
            { typ = t "Nil a, Nil b, CProd p => p a b -> a"
            , impl = JsLambda ["x"] "x[0]"
            })
      ]
    lalaItems
      -- HOFs
     =
      [ ( "Const"
        , Item
            { typ = t "Nil a, Nil b => a -> b -> a"
            , impl = LalaImpl ["a", "b"] (e "a")
            })
      , ("Id", Item {typ = t "Nil a => a -> a", impl = LalaImpl ["x"] (e "x")})
      , ( "Apply"
        , Item
            { typ = t "Nil a, Nil b => (a -> b) -> a -> b"
            , impl = LalaImpl ["f", "x"] (e "f x")
            })
      , ( "Compose"
        , Item
            { typ = t "Nil a, Nil b, Nil c => (b -> c) -> (a -> b) -> a -> c"
            , impl = LalaImpl ["f", "g", "x"] (e "f (g x)")
            })
      , ( "Short"
        , Item
            { typ = t "Nil a, Nil b => (a -> a -> b) -> a -> b"
            , impl = LalaImpl ["f", "x"] (e "f x x")
            })
      , ( "Flip"
        , Item
            { typ = t "Nil a, Nil b, Nil c => (a -> b -> c) -> b -> a -> c"
            , impl = LalaImpl ["f", "x", "y"] (e "f y x")
            })
      -- Numbers
      , ( "Inc"
        , Item {typ = t "CNum a => a -> a", impl = LalaImpl [] (e "Add 1")})
      -- Sequences
      , ( "Len"
        , Item
            { typ = t "CSeq s, Nil a, CNum n => s a -> n"
            , impl = LalaImpl [] (e "Foldl (Compose Const Inc) 0")
            })
      -- Pairs
      -- Inspired by Data.Bifunctor.
      , ( "Snd"
        , Item
            { typ = t "Nil a, Nil b, CProd p => p a b -> b"
            , impl = LalaImpl ["x"] (e "Fst (Swap x)")
            })
      , ( "Bimap"
        , Item
            { typ =
                t
                  "Nil a, Nil b, Nil c, Nil d, CProd p => (a -> c) -> (b -> d) -> p a b -> p c d"
            , impl =
                LalaImpl ["fa", "fb", "x"] (e "Pair (fa (Fst x)) (fb (Snd x))")
            })
      , ( "First"
        , Item
            { typ =
                t "Nil a, Nil b, Nil c, CProd p => (a -> c) -> p a b -> p c b"
            , impl = LalaImpl ["f"] (e "Bimap f Id")
            })
      , ( "Second"
        , Item
            { typ =
                t "Nil a, Nil b, Nil c, CProd p => (b -> c) -> p a b -> p a c"
            , impl = LalaImpl ["f"] (e "Bimap Id f")
            })
      , ( "Uncurry"
        , Item
            { typ =
                t "Nil a, Nil b, Nil c, CProd p => (a -> b -> c) -> p a b -> c"
            , impl = LalaImpl ["f", "p"] (e "f (Fst p) (Snd p)")
            })
      ]
