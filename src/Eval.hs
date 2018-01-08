module Eval (eval) where

import Control.Monad.Except (throwError)
import LispVal (LispVal(..))
import LispError
  ( LispError(..)
  , ThrowsError
  )

eval :: LispVal -> ThrowsError LispVal
eval val@(String _)             = return val
eval val@(Number _)             = return val
eval val@(Bool _)               = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func:args))    = 
  mapM eval args >>= apply func
eval badForm                    =
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
  [ ("+", numericBinaryOperator (+))
  , ("-", numericBinaryOperator (-))
  , ("*", numericBinaryOperator (*))
  , ("/", numericBinaryOperator div)
  , ("mod", numericBinaryOperator mod)
  , ("quotient", numericBinaryOperator quot)
  , ("remainder", numericBinaryOperator rem)
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
