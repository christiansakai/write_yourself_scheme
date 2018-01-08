module LispValSpec (test) where

import Test.Hspec 
  ( SpecWith
  , shouldBe
  , describe
  , it
  )
import LispVal 
  ( LispVal(..)
  , eval
  )

test :: SpecWith ()
test = 
  describe "LispVal" $ do
    it "show instance" $ do
      let str = String "Hello"
          atom = Atom "leet"
          num = Number 123
          true = Bool True
          false = Bool False

          list = List [atom, str, num]
          dottedList = DottedList [atom, str] false

      show str `shouldBe` "\"Hello\""
      show atom `shouldBe` "leet"
      show num `shouldBe` "123"
      show true `shouldBe` "#t"
      show false `shouldBe` "#f"
      show list `shouldBe` "(leet \"Hello\" 123)"
      show dottedList `shouldBe` "(leet \"Hello\" . #f)"

    it "eval LispVal" $ do
      eval (String "hello") `shouldBe` String "hello"
      eval (Number 1) `shouldBe` Number 1
      eval (Bool True) `shouldBe` Bool True
      eval (List [Atom "quote", Bool False]) `shouldBe` Bool False
      eval (List [Atom "+", Number 1, Number 2]) `shouldBe` Number 3






