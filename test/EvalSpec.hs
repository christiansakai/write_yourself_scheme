module EvalSpec (test) where

import Test.Hspec 
  ( SpecWith
  , shouldBe
  , describe
  , it
  )
import Eval (eval) 
import LispVal (LispVal(..))
import LispError (LispError(..))

test :: SpecWith ()
test = 
  describe "Eval" $ do
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

    it "throws error for Dotted List" $ do
      let dottedList = DottedList [Atom "+", Number 2] (Number 2)

      eval dottedList
        `shouldBe` 
          Left (BadSpecialForm 
                "Unrecognized special form" 
                dottedList)

--     -- it "eval LispVal" $ do
--     --   eval (String "hello") `shouldBe` String "hello"
--     --   eval (Number 1) `shouldBe` Number 1
--     --   eval (Bool True) `shouldBe` Bool True
--     --   eval (List [Atom "quote", Bool False]) `shouldBe` Bool False
--     --   eval (List [Atom "+", Number 1, Number 2]) `shouldBe` Number 3


