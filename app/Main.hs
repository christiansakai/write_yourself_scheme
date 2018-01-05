module Main where

import Parser

main :: IO ()
main = do
  expr <- getLine
  print $ readExpr expr


-- Parser


