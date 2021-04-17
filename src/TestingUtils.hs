module TestingUtils where

import Control.Monad ((>=>))
import Data.Either (fromRight)

import qualified LalaType
import qualified Parse

import DynamicExpression (DynamicExpression)
import Expression (Expression)
import LalaType (LalaType)
import Type

unwrapOrDie id (Right x) = x
unwrapOrDie id (Left e) = error (id ++ " - " ++ show e)

parseTypeOrDie :: String -> LalaType
parseTypeOrDie =
  unwrapOrDie "parseTypeOrDie" . (Parse.parseType >=> LalaType.fromParsedType)

parseDynamicExpressionOrDie :: String -> DynamicExpression
parseDynamicExpressionOrDie =
  unwrapOrDie "parseDynamicExpressionOrDie" . Parse.parseDynamicExpression

parseExpressionOrDie :: String -> Expression
parseExpressionOrDie =
  unwrapOrDie "parseExpressionOrDie" . Parse.parseExpression

numTypeDef = parseTypeOrDie "CNum a => a"

strTypeDef = parseTypeOrDie "CStr a => a"

incTypeDef = parseTypeOrDie "CNum a => a -> a"

addTypeDef = parseTypeOrDie "CNum a => a -> a -> a"
