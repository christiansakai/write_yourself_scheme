module Parser 
  ( readExpr 
  , parseString
  , parseAtom
  , parseNumber
  , parseExpr
  , parseList
  , parseDottedList
  , parseQuoted
  ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad (liftM)
import LispVal

readExpr :: String -> String
readExpr input =
  case parse parseExpr "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value " ++ show val

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  str <- many (noneOf "\"")
  char '"'
  return $ String str

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)

  let atom = first:rest

  return $ 
    case atom of
      "#t" -> Bool True
      "#f" -> Bool False
      _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do 
  digits <- many1 digit
  return . Number . read $ digits

parseExpr :: Parser LispVal
parseExpr = 
      parseAtom
  <|> parseString
  <|> parseNumber
  <|> parseQuoted
  <|> do
        char '('
        list <- try parseList <|> parseDottedList
        char ')'
        return list
        
parseList :: Parser LispVal
parseList = liftM List $ parseExpr `sepBy` spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- parseExpr `endBy` spaces
  tail <- do
      char '.'
      spaces
      parseExpr

  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  expr <- parseExpr
  return $ List [Atom "quote", expr]


