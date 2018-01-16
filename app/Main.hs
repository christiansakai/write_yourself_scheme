module Main where

import System.Environment (getArgs)
import Control.Monad (liftM)
import Eval (evaluate)
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

