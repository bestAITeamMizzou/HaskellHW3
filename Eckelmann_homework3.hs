{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Homework3 where

import Test.Hspec
import Control.Exception (evaluate)
import RPNAST


--words prelude function breaks a string up by spaces
--tokenizing is "lexing"

-- Function prob1
-- @type    prob1 :: String -> PExp
-- @param   String
-- @output  PExp
-- @description: Takes a string, passes the results of calling words on it to the helper function, and returns the resulting PExp. Does nothing to handle bad input.
--               The helper function compares the strings to the valid possibilities, creating a new list recursively based on what matches.
prob1   :: String -> PExp
prob1 x = prob1_helper(words(x))
    where prob1_helper :: [String] -> [Op]
          prob1_helper [] = []
          prob1_helper (x:xs)
            | x == "+"    = Plus    : prob1_helper(xs)
            | x == "-"    = Minus   : prob1_helper(xs)
            | x == "*"    = Mul     : prob1_helper(xs)
            | x == "/"    = IntDiv  : prob1_helper(xs)
            | otherwise   = (Val (read x :: Int)) : prob1_helper(xs)

-- Function prob2
-- @type    prob2 :: PExp -> Int
-- @param   PExp
-- @output  Int
-- @description: Takes a PExp, passes it to the helper function, and returns the result. Does not handle errors gracefully.
--               The helper function 'loops' through the given PExp, looking at the first element in the list. If its a value, it puts ontop of a stack, if it's 
--               not a value, it then takes the top two elements from the stack, evaluates them, and puts the resulting value ontop of the stack. Continues until 
--               the PExp is empty, or nothing matches and it reaches the catch all pattern for Bad Input. 
prob2   :: PExp -> Int
prob2   = prob2_helper [] --The PExp ('y' in the helper function) is passed to it implicitly
    where prob2_helper :: [Int] -> PExp -> Int
          prob2_helper (y:[]) []            = y
          prob2_helper y ((Val x):xs)       = prob2_helper (x : y) xs
          prob2_helper (y:z:zs) (Plus:xs)   = prob2_helper ((z + y):zs) xs
          prob2_helper (y:z:zs) (Minus:xs)  = prob2_helper ((z - y):zs) xs
          prob2_helper (y:z:zs) (Mul:xs)    = prob2_helper ((z * y):zs) xs
          prob2_helper (y:z:zs) (IntDiv:xs)
            | y == 0    = error("Cannot divide by zero!")
            | otherwise = prob2_helper ((z `div` y):zs) xs
          prob2_helper _ _                  = error("Bad Input.")

-- Function prob3
-- @type    prob3 :: PExp -> RPNResult
-- @param   PExp
-- @output  RPNResult
-- @description: Same as problem 2, expect that it fails with grace.
prob3   :: PExp -> RPNResult
prob3   = prob3_helper [] --The PExp ('y' in the helper function) is passed to it implicitly
    where prob3_helper :: [Int] -> PExp -> RPNResult
          prob3_helper (y:[]) []            = Success y
          prob3_helper y ((Val x):xs)       = prob3_helper (x : y) xs
          prob3_helper (y:z:zs) (Plus:xs)   = prob3_helper ((z + y):zs) xs
          prob3_helper (y:z:zs) (Minus:xs)  = prob3_helper ((z - y):zs) xs
          prob3_helper (y:z:zs) (Mul:xs)    = prob3_helper ((z * y):zs) xs
          prob3_helper (y:z:zs) (IntDiv:xs)
            | y == 0    = Failure DivByZero
            | otherwise = prob3_helper ((z `div` y):zs) xs
          prob3_helper _ _                  = Failure BadSyntax

-- Function prob4
-- @type    prob4 :: PExp -> Result String String
-- @param   PExp
-- @output  Result String String
-- @description: Takes the given PExp, and passes it to the helper function, and returns the results.
--               The helper function operates similarly to that of problems 2 and 3. However, rather than actually evaluating the expressions, it 'evaluates'
--               them by converting them into the appropriate string representation, and putting that on a stack, building up a larger and larger expression
--               as it goes until the PExp is empty, or it reaches the catch all pattern for Bad Input.
prob4   :: PExp -> Result String String
prob4   = prob4_helper [] --The PExp ('y' in the helper function) is passed to it implicitly
    where prob4_helper :: [String] -> PExp -> Result String String
          prob4_helper (y:[]) []            = Success y
          prob4_helper y ((Val x):xs)       = prob4_helper ((show x ): y) xs
          prob4_helper (y:z:zs) (Plus:xs)   = prob4_helper (("(" ++ z ++ " + " ++ y ++ ")"):zs) xs
          prob4_helper (y:z:zs) (Minus:xs)  = prob4_helper (("(" ++ z ++ " - " ++ y ++ ")"):zs) xs
          prob4_helper (y:z:zs) (Mul:xs)    = prob4_helper (("(" ++ z ++ " * " ++ y ++ ")"):zs) xs
          prob4_helper (y:z:zs) (IntDiv:xs) = prob4_helper (("(" ++ z ++ " / " ++ y ++ ")"):zs) xs
          prob4_helper _ _                  = Failure "Bad Input."

-- Write your Hspec Tests below

test_probs :: IO()
test_probs = do
  test_prob1
  test_prob2
  test_prob3
  test_prob4

-- got some tests from a wikipedia article https://en.wikipedia.org/wiki/Reverse_Polish_notation#Example
  
test_prob1 :: IO()
test_prob1 = hspec $ do
  describe "prob1(parser)" $ do
    context "when provided with valid input" $ do
      it "returns a PExp" $ do
        prob1 "200 + - * /" `shouldBe` [Val 200, Plus, Minus, Mul, IntDiv]
      it "returns a PExp" $ do
        prob1 "15 7 1 1 + - / 3 * 2 1 1 + + -" `shouldBe` [Val 15, Val 7, Val 1, Val 1, Plus, Minus, IntDiv, Val 3, Mul, Val 2, Val 1, Val 1, Plus, Plus, Minus]
        --prob1 "15 7 1 1 + − ÷ 3 × 2 1 1 + + −" `shouldBe` [Val 15, Val 7, Val 1, Val 1, Plus, Minus, IntDiv, Val 3, Mul, Val 2, Val 1, Val 1, Plus, Plus, Minus]


test_prob2 :: IO()
test_prob2 = hspec $ do
  describe "prob2(evaluation)" $ do
    context "when provided with valid input" $ do
      it "returns an Int" $ do
        prob2 [Val 4, Val 2, IntDiv] `shouldBe` 2
      it "returns an Int" $ do
        prob2 [Val 15, Val 7, Val 1, Val 1, Plus, Minus, IntDiv, Val 3, Mul, Val 2, Val 1, Val 1, Plus, Plus, Minus] `shouldBe` 5
    context "when provided with syntactically incorrect input" $ do
      it "throws an error" $ do
        evaluate (prob2 [Mul]) `shouldThrow` errorCall "Bad Input."
    context "when provided with an expression that tries to divide by 0" $ do
      it "throws an error" $ do
        evaluate(prob2 [Val 4, Val 0, IntDiv]) `shouldThrow` errorCall "Cannot divide by zero!"
        
test_prob3 :: IO()
test_prob3 = hspec $ do
  describe "prob3(evaluation)" $ do
    context "when provided with valid input" $ do
      it "returns an Int" $ do
        prob3 [Val 4, Val 2, IntDiv] `shouldBe` Success 2
      it "returns an Int" $ do
        prob3 [Val 15, Val 7, Val 1, Val 1, Plus, Minus, IntDiv, Val 3, Mul, Val 2, Val 1, Val 1, Plus, Plus, Minus] `shouldBe` Success 5
    context "when provided with syntactically incorrect input" $ do
      it "throws an error" $ do
        prob3 [IntDiv, Plus, Val 0] `shouldBe` Failure BadSyntax
    context "when provided with an expression that tries to divide by 0" $ do
      it "throws an error" $ do
        prob3 [Val 5, Val 0, IntDiv] `shouldBe` Failure DivByZero
        
test_prob4 :: IO()
test_prob4 = hspec $ do
  describe "prob4(infix converter)" $ do
    context "When provided with valid input" $ do
      it "returns an infix string." $ do
        prob4 [Val 1, Val 1, Plus] `shouldBe` Success "(1 + 1)"
      it "returns an infix string." $ do
        prob4 [Val 2, Val 4, Plus, Val 3, IntDiv] `shouldBe` Success "((2 + 4) / 3)"
      it "returns an infix string." $ do
        prob4 [Val 2] `shouldBe` Success "2"
      it "returns an infix string." $ do
        prob4 [Val 15, Val 7, Val 1, Val 1, Plus, Minus, IntDiv, Val 3, Mul, Val 2, Val 1, Val 1, Plus, Plus, Minus] `shouldBe` Success "(((15 / (7 - (1 + 1))) * 3) - (2 + (1 + 1)))"
        --prob4 [Val 15, Val 7, Val 1, Val 1, Plus, Minus, IntDiv, Val 3, Mul, Val 2, Val 1, Val 1, Plus, Plus, Minus] `shouldBe` Success "((15 ÷ (7 − (1 + 1))) × 3) − (2 + (1 + 1))"
    context "When provided with an invalid expression" $ do
      it "returns Failure" $ do
        prob4 [Plus] `shouldBe` Failure "Bad Input."
      it "returns Failure" $ do
        prob4 [Val 1, Val 2] `shouldBe` Failure "Bad Input."
      it "returns Failure" $ do
        prob4 [Val 1, Val 2, Val 3, Plus] `shouldBe` Failure "Bad Input."
      it "returns Failure" $ do
        prob4 [Val 2, Plus, Val 3] `shouldBe` Failure "Bad Input."