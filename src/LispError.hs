module LispError 
  ( LispError(..)
  , ThrowsError
  , trapError
  , extractValue
  ) where

import Text.ParserCombinators.Parsec
import Control.Monad.Except
  ( MonadError
  , catchError
  )
import LispVal
  ( LispVal(..)
  , unwordsList
  )

data LispError
  = NumArgs Integer [LispVal]
  | TypeMisMatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String 
  | UnboundVar String String
  | Default String
  deriving Eq

instance Show LispError where
  -- show :: a => String a
  show = showError

showError :: LispError -> String
showError (UnboundVar msg varname)      = msg ++ ": " ++ varname
showError (BadSpecialForm msg form)     = msg ++ ": " ++ show form
showError (NotFunction msg func)        = msg ++ ": " ++ show func
showError (NumArgs expected found)      =
  "Expected " ++ show expected ++ " args; "
  ++ "found values " ++ unwordsList found
showError (TypeMisMatch expected found) =
  "Invalid type: expected " ++ expected ++ ", found "
  ++ show found
showError (Parser parseErr)             =
  "Parse error at " ++ show parseErr
showError (Default str)                 = str

type ThrowsError = Either LispError

trapError :: (Show a, MonadError a m)
          => m String -> m String
trapError action = 
  catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
