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
  testEvaluatesFunctions
  testEvaluatesCons
  testThrowError

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

    it "evaluates -" $ do
      evaluate "(+ -2 2)" `shouldBe` Right "Right 0"

    it "evaluates recursively" $ do
      evaluate "(+ 2 (- 4 1))" `shouldBe` Right "Right 5"

    it "evaluates recursively deeper" $ do
      evaluate "(- (+ 4 6 3) 3 5 2)" `shouldBe` Right "Right 3"

testEvaluatesFunctions :: SpecWith ()
testEvaluatesFunctions = do
  describe "Evaluate - evaluates functions" $ do
    it "evaluates +" $ do
      evaluate "(+ 1 2)" `shouldBe` Right "Right 3"

    it "evaluates -" $ do
      evaluate "(- 2 1)" `shouldBe` Right "Right 1"

    it "evaluates - negative" $ do
      evaluate "(- 1 2)" `shouldBe` Right "Right -1"

    it "evaluates *" $ do
      evaluate "(* 3 6)" `shouldBe` Right "Right 18"
      
    it "evaluates /" $ do
      evaluate "(/ 12 4)" `shouldBe` Right "Right 3"

    it "evaluates mod" $ do
      evaluate "(mod 10 3)" `shouldBe` Right "Right 1"

    it "evaluates quotient" $ do
      evaluate "(quotient 10 3)" `shouldBe` Right "Right 3"

    it "evaluates ==" $ do
      evaluate "(== 10 3)" `shouldBe` Right "Right #f"

    it "evaluates <" $ do
      evaluate "(< 10 3)" `shouldBe` Right "Right #f"

    it "evaluates >" $ do
      evaluate "(> 10 3)" `shouldBe` Right "Right #t"

    it "evaluates /=" $ do
      evaluate "(/= 10 3)" `shouldBe` Right "Right #t"

    it "evaluates >=" $ do
      evaluate "(>= 10 3)" `shouldBe` Right "Right #t"

    it "evaluates >=" $ do
      evaluate "(>= 3 3)" `shouldBe` Right "Right #t"

    it "evaluates <=" $ do
      evaluate "(<= 3 3)" `shouldBe` Right "Right #t"

    it "evaluates <=" $ do
      evaluate "(<= 3 4)" `shouldBe` Right "Right #t"

    it "evaluates &&" $ do
      evaluate "(&& #t #t)" `shouldBe` Right "Right #t"

    it "evaluates &&" $ do
      evaluate "(&& #t #f)" `shouldBe` Right "Right #f"

    it "evaluates &&" $ do
      evaluate "(&& #f #t)" `shouldBe` Right "Right #f"

    it "evaluates &&" $ do
      evaluate "(&& #f #f)" `shouldBe` Right "Right #f"

    it "evaluates ||" $ do
      evaluate "(|| #t #t)" `shouldBe` Right "Right #t"

    it "evaluates ||" $ do
      evaluate "(|| #t #f)" `shouldBe` Right "Right #t"

    it "evaluates ||" $ do
      evaluate "(|| #f #t)" `shouldBe` Right "Right #t"

    it "evaluates ||" $ do
      evaluate "(|| #f #f)" `shouldBe` Right "Right #f"

    it "evaluates string=?" $ do
      evaluate "(string=? \"hello\" \"hello\")"
        `shouldBe` Right "Right #t"

    it "evaluates string>?" $ do
      evaluate "(string>? \"hello\" \"hallo\")"
        `shouldBe` Right "Right #t"

    it "evaluates string<=?" $ do
      evaluate "(string<=? \"hallo\" \"hello\")"
        `shouldBe` Right "Right #t"

    it "evaluates string<=?" $ do
      evaluate "(string<=? \"hello\" \"hello\")"
        `shouldBe` Right "Right #t"

    it "evaluates string>=?" $ do
      evaluate "(string>=? \"hello\" \"hallo\")"
        `shouldBe` Right "Right #t"

    it "evaluates string>=?" $ do
      evaluate "(string>=? \"hello\" \"hello\")"
        `shouldBe` Right "Right #t"

testEvaluatesCons :: SpecWith ()
testEvaluatesCons = do
  describe "Evaluate - cons" $ do
    it "evaluates cons to list" $ do
      evaluate "(cons 1 ())" 
        `shouldBe` Right "Right (1)"

    it "evaluates cons to list" $ do
      evaluate "(cons 1 (cons 2 ()))"
        `shouldBe` Right "Right (1 2)"

    it "evaluates cons to list" $ do
      evaluate "(cons 1 '(2 3))" 
        `shouldBe` Right "Right (1 2 3)"
   
    it "evaluates cons to dotted list" $ do
      evaluate "(cons 1 2)" 
        `shouldBe` Right "Right (1 . 2)"

testThrowError :: SpecWith ()
testThrowError = do
  describe "Evaluate - throws error" $ do
    it "for bad form" $ do
      evaluate "(1 2 2)" 
        `shouldBe` 
          Right "Left Unrecognized special form: (1 2 2)"

    it "for wrong type" $ do 
      evaluate "(+ 2 \"two\")"
        `shouldBe`
          Right "Left Invalid type: expected number, found \"two\""

    it "for not enough arguments" $ do
      evaluate "(+ 3)"
        `shouldBe`
          Right "Left Expected 2 args; found values 3"

    it "unrecognized function" $ do
      evaluate "(what? 2)"
        `shouldBe`
          Right "Left Unrecognized primitive function args: \"what?\""
