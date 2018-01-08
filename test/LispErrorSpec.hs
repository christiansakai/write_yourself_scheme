module LispErrorSpec where

import Text.ParserCombinators.Parsec
  ( ParseError
  , parse
  , digit
  )
import Test.Hspec 
  ( SpecWith
  , shouldBe
  , describe
  , it
  )
import LispError 
  ( LispError(..)
  , trapError
  , extractValue
  )
import LispVal (LispVal(..))

test :: SpecWith ()
test = 
  describe "LispError" $ do
    it "show instance" $ do
      let unbound = UnboundVar "unbound" "leet"
          badForm = BadSpecialForm "bad" (Atom "hello")
          notFunc = NotFunction "notFunc" "<|>"
          numArgs = NumArgs 3 [Atom "haha"]
          typeMis = TypeMisMatch "typeMis" (String "2")
          defErr  = Default "default msg"
          parseErr = 
            Parser $ let (Left err) = parse digit "lisp" "a"
                      in err

      show unbound `shouldBe` "unbound: leet"
      show badForm `shouldBe` "bad: hello"
      show notFunc `shouldBe` "notFunc: \"<|>\""
      show numArgs `shouldBe` 
        "Expected 3 args; found values haha"
      show typeMis `shouldBe` 
        "Invalid type: expected typeMis, found \"2\""
      show defErr `shouldBe` "default msg"
      show parseErr `shouldBe`
        ("Parse error at \"lisp\" (line 1, column 1):\n"
         ++ "unexpected \"a\"\n" 
         ++ "expecting digit")

--     -- it "eval LispVal" $ do
--     --   eval (String "hello") `shouldBe` String "hello"
--     --   eval (Number 1) `shouldBe` Number 1
--     --   eval (Bool True) `shouldBe` Bool True
--     --   eval (List [Atom "quote", Bool False]) `shouldBe` Bool False
--     --   eval (List [Atom "+", Number 1, Number 2]) `shouldBe` Number 3






