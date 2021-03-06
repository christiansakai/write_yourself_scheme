module LispVal
  ( LispVal(..)
  , unwordsList
  ) where

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Nil
  deriving Eq

instance Show LispVal where
  -- show :: a -> String
  show = showVal

showVal :: LispVal -> String
showVal (String contents)       = "\"" ++ contents ++ "\""
showVal (Atom name)             = name
showVal (Number contents)       = show contents
showVal (Bool True)             = "#t"
showVal (Bool False)            = "#f"
showVal (List contents)         = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail)  = "(" ++ unwordsList head ++ " . "
                               ++ showVal tail ++ ")"
showVal Nil                     = ""

unwordsList :: [LispVal] -> String
unwordsList = unwords . fmap showVal
