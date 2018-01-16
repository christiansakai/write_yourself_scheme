module Eval 
  ( eval
  , evaluate
  ) where

import Control.Monad.Except (throwError)
import Parser (readExpr)
import LispVal (LispVal(..))
import LispError
  ( LispError(..)
  , ThrowsError
  )

evaluate :: String -> ThrowsError String
evaluate expr = do
  valOrErr <- readExpr expr
  return . show . eval $ valOrErr

eval :: LispVal -> ThrowsError LispVal
eval val@(String _)                         = return val
eval val@(Number _)                         = return val
eval val@(Bool _)                           = return val
eval (List [Atom "quote", val])             = return val
eval (List [Atom "if", pred, conseq, alt])  = do
  result <- eval pred
  case result of 
    Bool False  -> eval alt
    otherwise   -> eval conseq
eval (List (Atom func:args))                = 
  mapM eval args >>= apply func
eval badForm                                =
  throwError $ BadSpecialForm 
    "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe defaultVal applyOp maybeOp
  where
    defaultVal =
      throwError $ NotFunction
        "Unrecognized primitive function args" func

    maybeOp = lookup func operators
    applyOp op = op $ args

operators :: [(String, [LispVal] -> ThrowsError LispVal)]
operators =
  [ ("+",         numericBinaryOperator     (+))
  , ("-",         numericBinaryOperator     (-))
  , ("*",         numericBinaryOperator     (*))
  , ("/",         numericBinaryOperator     div)
  , ("mod",       numericBinaryOperator     mod)
  , ("quotient",  numericBinaryOperator     quot)

  , ("==",        numericBoolBinaryOperator (==))
  , ("<",         numericBoolBinaryOperator (<))
  , (">",         numericBoolBinaryOperator (>))
  , ("/=",        numericBoolBinaryOperator (/=))
  , (">=",        numericBoolBinaryOperator (>=))
  , ("<=",        numericBoolBinaryOperator (<=))

  , ("&&",        booleanBoolBinaryOperator (&&))
  , ("||",        booleanBoolBinaryOperator (||))

  , ("string=?",  stringBoolBinaryOperator  (==))
  , ("string?",   stringBoolBinaryOperator  (>))
  , ("string<=?", stringBoolBinaryOperator  (<=))
  , ("string>=?", stringBoolBinaryOperator  (>=))
  ]

numericBinaryOperator :: (Integer -> Integer -> Integer)
                      -> [LispVal]
                      -> ThrowsError LispVal
numericBinaryOperator op []             = 
  throwError $ NumArgs 2 []
numericBinaryOperator op singleVal@[_]  =  
  throwError $ NumArgs 2 singleVal
numericBinaryOperator operator params   = 
  mapM unpackToNum params >>= 
    return . Number . foldl1 operator

unpackToNum :: LispVal -> ThrowsError Integer
unpackToNum (Number n)  = return n
unpackToNum (String n)  =
  let parsed = reads n :: [(Integer, String)]
   in if null parsed
        then throwError $
          TypeMisMatch "number" $ String n
        else return $ fst $ parsed !! 0
unpackToNum (List [n])  = unpackToNum n
unpackToNum notNum      = 
  throwError $ TypeMisMatch "number" notNum

boolBinaryOp :: (LispVal -> ThrowsError a)
             -> (a -> a -> Bool)
             -> [LispVal]
             -> ThrowsError LispVal
boolBinaryOp unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      let [left, right] = args

      left' <- unpacker left
      right' <- unpacker right

      return $ Bool $ left' `op` right'

numericBoolBinaryOperator = boolBinaryOp unpackToNum

stringBoolBinaryOperator = boolBinaryOp unpackToStr

booleanBoolBinaryOperator = boolBinaryOp unpackToBool

unpackToStr :: LispVal -> ThrowsError String
unpackToStr (String s)  = return s
unpackToStr (Number n)  = return . show $ n
unpackToStr (Bool b)    = return . show $ b
unpackToStr notString   =
  throwError $ TypeMisMatch "string" notString

unpackToBool :: LispVal -> ThrowsError Bool
unpackToBool (Bool b) = return b
unpackToBool notBool  =
  throwError $ TypeMisMatch "boolean" notBool
