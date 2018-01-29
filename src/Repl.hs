module Repl 
  ( runRepl
  , evalAndPrint
  ) where

import System.IO
  ( hFlush
  , stdout
  )
import Control.Monad (liftM)
import Control.Monad.Except 
  ( ExceptT
  , throwError
  , runExceptT
  )
import Parser (readExpr)
import LispVal ( LispVal(..))
import Eval (eval)
import LispError
  ( extractValue
  , trapError
  , LispError(..)
  , ThrowsError
  )

flushStr :: String -> IO ()
flushStr str =
  putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt =
  flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr =
  return $ extractValue $ trapError
    (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr =
  evalString expr >>= putStrLn

until_ :: Monad m 
      => (a -> Bool)
      -> m a 
      -> (a -> m ())
      -> m ()
until_ predicate prompt action = do
  result <- prompt
  if predicate result
     then return ()
     else do 
       action result 
       until_ predicate prompt action
            
runRepl :: IO ()
runRepl = 
  until_ 
    (== "quit") 
    (readPrompt "Lsp>>> ")
    evalAndPrint
