module EvalSpec (test) where

import Test.Hspec 
  ( SpecWith
  , shouldBe
  , describe
  , it
  )
import Eval 
  ( eval
  , evaluate
  ) 
import LispVal (LispVal(..))
import LispError 
  ( LispError(..)
  , ThrowsError 
  )

test :: SpecWith ()
test = do
  testEval
  testEvaluate

testEval :: SpecWith ()
testEval = do
  testPrimitive
  testFunctions

testPrimitive :: SpecWith ()
testPrimitive = 
  describe "Eval - primitives" $ do
    it "evals String" $ do
      eval (String "hello")
        `shouldBe` Right (String "hello")

    it "evals Number" $ do
      eval (Number 3)
        `shouldBe`  Right (Number 3)

    it "evals Bool" $ do
      eval (Bool True)
        `shouldBe` Right (Bool True)

    it "evals Quoted List" $ do
      eval (List [Atom "quote", Number 2])
        `shouldBe` Right (Number 2)

    it "throws error bad form" $ do
      let dottedList = DottedList [Atom "+", Number 2] (Number 2)

      eval dottedList
        `shouldBe` 
          Left (BadSpecialForm 
                "Unrecognized special form" 
                dottedList)

testFunctions :: SpecWith ()
testFunctions =
  describe "Eval - functions" $ do
    it "evals Function as List" $ do
      eval (List [Atom "+", Number 2, Number 3])
        `shouldBe` Right (Number 5)
  
    it "throws error for empty argument" $ do
      eval (List [Atom "-"]) 
        `shouldBe` Left (NumArgs 2 [])

    it "throws error for not enough argument" $ do
      eval (List [Atom "+", Number 2]) 
        `shouldBe` Left (NumArgs 2 [Number 2])

    it "throws error for unrecognized function" $ do
      eval (List [Atom "haha", Number 2]) 
        `shouldBe` 
          Left (NotFunction 
                "Unrecognized primitive function args" 
                "haha")

    it "evals String if it is valid number" $ do
      eval (List [Atom "+", String "2", String "3"])
        `shouldBe` Right (Number 5)

    it "throws error if String is invalid number" $ do
      eval (List [Atom "+", String "a2", String "3"])
        `shouldBe` 
          Left (TypeMisMatch 
                "number" 
                (String "a2"))

testEvaluate :: SpecWith ()
testEvaluate = do
  testEvaluates
  -- testEvaluatesFunctions
  -- testThrowError

testEvaluates :: SpecWith ()
testEvaluates = do
  describe "Evaluate - evaluates" $ do
    it "evaluates a quote" $ do
      evaluate "'atom" `shouldBe` Right "Right atom"

    it "evaluates a number" $ do
      evaluate "2" `shouldBe` Right "Right 2"

    it "evaluates a string" $ do
      evaluate "\"a string\"" `shouldBe` Right "Right \"a string\""

    it "evaluates +" $ do
      evaluate "(+ 2 2)" `shouldBe` Right "Right 4"

    it "evaluates recursively" $ do
      evaluate "(+ 2 (- 4 1))" `shouldBe` Right "Right 5"

    it "evaluates recursively deeper" $ do
      evaluate "(- (+ 4 6 3) 3 5 2)" `shouldBe` Right "Right 3"








testThrowError :: SpecWith ()
testThrowError = undefined
    -- it "evaluates recursively" $ do
    --   evaluate "(+ 2 (-4 1))" `shouldBe` Right "Right 2"

    -- it "evaluates number" $ do
    --   evaluate "(1 2 2)" 
    --     `shouldBe` 
    --       Left (BadSpecialForm 
    --             "Unrecognized special form"
    --             (List [Atom "1", Atom "2", Atom "2"]))

--
--
-- $ ghc -package parsec -o errorcheck [../code/listing5.hs listing5.hs]
-- $ ./errorcheck "(+ 2 \"two\")"
-- Invalid type: expected number, found "two"
-- $ ./errorcheck "(+ 2)"
-- Expected 2 args; found values 2
-- $ ./errorcheck "(what? 2)"
-- Unrecognized primitive function args: "what?"
--
--
--
--
-- $ ghc -package parsec -o simple_parser [../code/listing6.1.hs listing6.1.hs]
-- $ ./simple_parser "(< 2 3)"
-- #t
-- $ ./simple_parser "(> 2 3)"
-- #f
-- $ ./simple_parser "(>= 3 3)"
-- #t
-- $ ./simple_parser "(string=? \"test\"  \"test\")"
-- #t
-- $ ./simple_parser "(string<? \"abc\" \"bba\")"
-- #t
--
--
-- $ ghc -package parsec -o simple_parser [../code/listing6.2.hs listing6.2.hs]
-- $ ./simple_parser "(if (> 2 3) \"no\" \"yes\")"
-- "yes"
-- $ ./simple_parser "(if (= 3 3) (+ 2 3 (- 5 1)) \"unequal\")"
-- 9
