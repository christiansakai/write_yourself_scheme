module Main where

import Test.Hspec (hspec)
import qualified ParserSpec as Parser
import qualified LispValSpec as LispVal
import qualified LispErrorSpec as LispError
import qualified EvalSpec as Eval

main :: IO ()
main = hspec $ do
  Parser.test
  LispVal.test
  LispError.test
  Eval.test
