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
import Repl 
  ( runRepl
  , evalAndPrint
  )

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0           -> runRepl
    1           -> evalAndPrint $ args !! 0
    otherwise   ->
      putStrLn "Program takes only 0 or 1 argument"
