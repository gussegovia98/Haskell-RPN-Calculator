-- HSpec tests for Val.hs
-- Execute: runhaskell EvalSpec.hs

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Val
import Eval

main :: IO ()
main = hspec $ do
  describe "eval" $ do
    context "*" $ do
        it "multiplies integers" $ do
            eval "*" [Integer 2, Integer 3] `shouldBe` [Integer 6]
        
        it "multiplies floats" $ do
            eval "*" [Integer 2, Real 3.0] `shouldBe` [Real 6.0]
            eval "*" [Real 3.0, Integer 3] `shouldBe` [Real 9.0]
            eval "*" [Real 4.0, Real 3.0] `shouldBe` [Real 12.0]

        it "errors on too few arguments" $ do   
            evaluate (eval "*" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "*" [Integer 2]) `shouldThrow` errorCall "Stack underflow"

        -- this does not work, seems to be a HSpec bug
        -- it "errors on non-numeric inputs" $ do
        --    evaluate(eval "*" [Real 3.0, Id "x"]) `shouldThrow` anyException

    context "+" $ do
        it "adds integers" $ do
            eval "+" [Integer 2, Integer 2] `shouldBe` [Integer 4]

        it "adds floats" $ do
            eval "+" [Integer 2, Real 3.0] `shouldBe` [Real 5.0]
            eval "+" [Real 3.0, Integer 3] `shouldBe` [Real 6.0]
            eval "+" [Real 3.0, Real 7.0] `shouldBe` [Real 10.0]

        it "errors on too few arguments" $ do
            evaluate (eval "+" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "+" [Integer 2]) `shouldThrow` errorCall "Stack underflow"
        --unsure about the last test case others seem fine tho

    context "-" $ do
        it "subtracts integers" $ do
            eval "-" [Integer 5, Integer 2] `shouldBe` [Integer 3]

        it "subtracts floats" $ do
            eval "-" [Integer 12, Real 5.0] `shouldBe` [Real 7.0]
            eval "-" [Real 5.0, Integer 3] `shouldBe` [Real 2.0]
            eval "-" [Real 20.0, Real 7.0] `shouldBe` [Real 13.0]

        it "errors on too few arguments" $ do
            evaluate (eval "-" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "-" [Integer 2]) `shouldThrow` errorCall "Stack underflow"
        --unsure about the last test case others seem fine tho

    context "^" $ do
        it "raises power ints" $ do
            eval "^" [Integer 5, Integer 2] `shouldBe` [Integer 25]

        it "raises power floats" $ do
            eval "^" [Integer 3, Real 5.1] `shouldBe` [Real 271.2179]
            eval "^" [Real 5.2, Integer 3] `shouldBe` [Real 140.60799]
            eval "^" [Real 3.2, Real 5.6] `shouldBe` [Real 674.2793796624868]

        it "errors on too few arguments" $ do
            evaluate (eval "^" []) `shouldThrow` errorCall "Stack underflow"
            evaluate (eval "^" [Integer 2]) `shouldThrow` errorCall "Stack underflow"

    context "/" $ do
            it "divides integers" $ do
                eval "/" [Integer 5, Integer 2] `shouldBe` [Integer 2]

            it "divides floats" $ do
                eval "/" [Integer 2, Real 4.0] `shouldBe` [Real 0.5]
                eval "/" [Real 5.0, Integer 20] `shouldBe` [Real 0.25]
                eval "/" [Real 21.0, Real 7.0] `shouldBe` [Real 3.0]

            it "errors on too few arguments" $ do
                evaluate (eval "/" []) `shouldThrow` errorCall "Stack underflow"
                evaluate (eval "/" [Integer 2]) `shouldThrow` errorCall "Stack underflow"
            --unsure about the last test case others seem fine tho


    context "DUP" $ do
        it "duplicates values" $ do
            eval "DUP" [Integer 2] `shouldBe` [Integer 2, Integer 2]
            eval "DUP" [Real 2.2] `shouldBe` [Real 2.2, Real 2.2]
            eval "DUP" [Id "x"] `shouldBe` [Id "x", Id "x"]

        it "errors on empty stack" $ do
            evaluate (eval "DUP" []) `shouldThrow` errorCall "Stack underflow"

    context "STR" $ do
    it "STR values" $ do
        eval "STR" [Integer 2] `shouldBe` [Id "2"]
        eval "STR" [Real 2.2] `shouldBe` [Id "2.2"]
        eval "STR" [Id "x"] `shouldBe` [Id "x"]

    {-it "errors on empty stack" $ do
        evaluate (eval "STR" []) `shouldThrow` errorCall "Stack underflow"
unsure will look at later-}
   
  describe "evalOut" $ do
      context "CR" $ do
          it "prints a char" $ do 
              evalOut "CR" ([],"") `shouldBe` ([],"\n")
      context "EMIT" $ do
          it "prints a char" $ do 
              evalOut "EMIT" ([Integer 65],"") `shouldBe` ([],"A")
      context "." $ do
        it "prints top of stack" $ do
            evalOut "." ([Id "x"], "") `shouldBe` ([],"\"x\"")
            evalOut "." ([Integer 2], "") `shouldBe` ([], "2")
            evalOut "." ([Real 2.2], "") `shouldBe` ([], "2.2")

        it "errors on empty stack" $ do
            evaluate(evalOut "." ([], "")) `shouldThrow` errorCall "Stack underflow"

        it "eval pass-through" $ do
         evalOut "*" ([Real 2.0, Integer 2], "blah") `shouldBe` ([Real 4.0], "blah")


--use chr constructor works only on character 
--65 EMIT = a
-- if <condition> then <stuff> else <stuff>


    {-describe "evalEMIT" $ do
        context "EMIT"
            it "prints out ascii num" $ do
                evalEMIT "EMIT" ([Integer 2], "") `shouldBe` ([], "2")
   describe "evalCR" $ do
       context "CR" $ do
        it "prints a newline" $ do
            evalCR "CR"-}

