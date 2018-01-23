module LispValSpec (test) where

import Test.Hspec 
  ( SpecWith
  , shouldBe
  , describe
  , it
  )
import LispVal 
  ( LispVal(..)
  , unwordsList
  )

test :: SpecWith ()
test = 
  describe "LispVal" $ do
    it "unwords List of LispVal" $ do
      unwordsList [Atom "leet", Number 1, Number 2]
        `shouldBe` "leet 1 2"

    it "show instance" $ do
      let str = String "Hello"
          atom = Atom "leet"
          num = Number 123
          nil = Nil
          true = Bool True
          false = Bool False

          list = List [atom, str, num]
          dottedList = DottedList [atom, str] false

      show str `shouldBe` "\"Hello\""
      show atom `shouldBe` "leet"
      show num `shouldBe` "123"
      show nil `shouldBe` ""
      show true `shouldBe` "#t"
      show false `shouldBe` "#f"
      show list `shouldBe` "(leet \"Hello\" 123)"
      show dottedList `shouldBe` "(leet \"Hello\" . #f)"
