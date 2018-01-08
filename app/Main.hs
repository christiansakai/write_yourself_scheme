module Main where

import System.Environment (getArgs)
import Control.Monad (liftM)
import Parser (readExpr)
import Eval (eval)
import LispError 
  ( ThrowsError
  , extractValue
  , trapError
  )
import LispVal (LispVal(..))

main :: IO ()
main = do
  expr <- liftM head getArgs

  evaluatedOrErr <- 
    return (evaluate expr) :: IO (ThrowsError String)

  putStrLn $ 
    extractValue $ 
      trapError evaluatedOrErr

evaluate :: String -> ThrowsError String
evaluate expr = do
  valOrErr <- readExpr expr
  return . show . eval $ valOrErr

