module ParserSpec where

import Test.Hspec
  ( SpecWith
  , describe
  , it
  , shouldBe
  )
import Text.ParserCombinators.Parsec (parse)
import Parser
  ( readExpr 
  , parseString
  , parseAtom
  , parseNumber
  , parseExpr
  , parseList
  , parseDottedList
  , parseQuoted
  )
import LispVal (LispVal(..))

test :: SpecWith ()
test = 
  describe "Parser" $ do
    let name = "lisp"

    it "parses String" $ do
      parse parseString name "\"Hello\"" 
        `shouldBe` Right (String "Hello")

    it "parses Atom" $ do
      parse parseAtom name "leet"
        `shouldBe` Right (Atom "leet")
