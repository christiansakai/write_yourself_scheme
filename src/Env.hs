module Env
  ( Env
  , IOThrowsError 
  , getVar
  , setVar
  ) where

import LispVal (LispVal(..))
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except
  ( ExceptT
  , throwError
  , runExceptT
  )
import Data.IORef
  ( IORef(..)
  , newIORef 
  , readIORef
  , writeIORef
  )
import LispError
  ( extractValue
  , trapError
  , LispError(..)
  , ThrowsError
  )

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action =
  runExceptT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound  envRef var =
  readIORef envRef >>=
    return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef

  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef

  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
        (liftIO . (flip writeIORef value))
        (lookup var env)

  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
     then setVar envRef var value >> return value
     else liftIO $ do
       valueRef <- newIORef value
       env <- readIORef envRef
       writeIORef envRef ((var, valueRef) : env)
       return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = do
  env <- readIORef envRef
  env' <- extendEnv bindings env
  env'' <- newIORef env'
  return env''

  where
    extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)
      


