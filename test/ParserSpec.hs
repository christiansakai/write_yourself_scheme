module ParserSpec (test) where

import Test.Hspec
  ( SpecWith
  , describe
  , it
  , shouldBe
  )
import Text.ParserCombinators.Parsec (parse)
import Parser
  ( parseString
  , parseAtom
  , parseNumber
  , parseList
  , parseDottedList
  , parseQuoted
  , parseExpr
  , readExpr
  )
import LispVal (LispVal(..))

test :: SpecWith ()
test = 
  describe "Parser" $ do
    let name = "lisp"

    it "parses String" $ do
      parse parseExpr name "\"Hello\"" 
        `shouldBe` Right (String "Hello")

    it "parses Atom" $ do
      parse parseExpr name "leet?"
        `shouldBe` Right (Atom "leet?")

    it "parses Boolean" $ do
      parse parseExpr name "#t"
        `shouldBe` Right (Bool True)

    it "parses Number" $ do
      parse parseExpr name "123"
        `shouldBe` Right (Number 123)

    it "parses List" $ do
      parse parseExpr name "(a 1 2)"
        `shouldBe` Right (List [Atom "a", Number 1, Number 2])

    it "parses Dotted List" $ do
      parse parseExpr name "(a 1 . 2)"
        `shouldBe` Right (DottedList [Atom "a", Number 1] (Number 2))

    it "parses Quoted Expression" $ do
      parse parseExpr name "'x"
        `shouldBe` Right (List [Atom "quote", Atom "x"])

