module Main where

import Test.Hspec (hspec)
import qualified LispValSpec as LispVal
import qualified ParserSpec as Parser

main :: IO ()
main = hspec $ do
  LispVal.test
  Parser.test


