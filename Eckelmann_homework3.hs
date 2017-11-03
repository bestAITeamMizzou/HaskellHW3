{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
--{-# LANGUAGE DeriveFoldable #-}
--{-# LANGUAGE DeriveFunctor #-}
--{-# LANGUAGE DeriveTraversable #-}
--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE LambdaCase #-}

module Homework3 where

import Test.Hspec
import RPNAST

prob1    :: String -> PExp
prob1    = undefined

prob2    :: PExp -> Int
prob2    = undefined

prob3    :: PExp -> Result String Int
prob3    = undefined

prob4    :: PExp -> Result String String
prob4    = undefined

-- Write your Hspec Tests below

test_probs :: IO()
test_probs = do
  test_prob1
  test_prob2
  test_prob3
  test_prob4

test_prob1 :: IO()
test_prob1 = hspec $ do
  describe "prob1(parser)" $ do
    context "when provided with invalid input" $ do
      it "returns a PExp" $ do
        prob1 "200 + - * /" `shouldBe` [Val 200, Plus, Minus, Mul, IntDiv]

test_prob2 :: IO()
test_prob2 = hspec $ do
  describe "prob2(evaluation)" $ do
    context "when provided with valid input" $ do
      it "returns a Int" $ do
        prob2 [Val 4, Val 2, IntDiv] `shouldBe` 2
{-   context "when provided with syntactically incorrect input" $ do
      it "throws an error" $ do
        prob2 [Mul] `shouldThrow` errorCall "Bad Input."
    context "when provided with an expression that tries to divide by 0" $ do
      it "throws an error" $ do
        prob2 [Val 4, Val 0, IntDiv] `shouldThrow` errorCall "Cannot divide by zero!"-}
		
test_prob3 :: IO()
test_prob3 = hspec $ do
  describe "prob2(evaluation)" $ do
    context "when provided with valid input" $ do
      it "returns a Int" $ do
        prob3 [Val 4, Val 2, IntDiv] `shouldBe` Success 2
    context "when provided with syntactically incorrect input" $ do
      it "throws an error" $ do
        prob3 [Mul] `shouldBe` Failure "Bad Input."
    context "when provided with syntactically incorrect input" $ do
      it "throws an error" $ do
        prob3 [Mul] `shouldBe` Failure "Cannot divide by zero!"
        
test_prob4 :: IO()
test_prob4 = undefined hspec $ do
  describe "prob4(infix converter)" $ do
    context "When provided with valid input" $ do
      it "returns an infix string." $ do
        prob4 [Val 1, Val 1, Plus] `shouldBe` Success "(1 + 1)"
      it "returns an infix string." $ do
        prob4 [Val 2, Val 4, Plus, Val 3, IntDiv] `shouldBe` Success "((2 + 4) / 3)"
      it "returns an infix string." $ do
        prob4 [Val 2] `shouldBe` Success "2"
    context "When provided with an invalid expression" $ do
      it "returns Failure" $ do
        prob4 [Plus] `shouldBe` Failure "Bad Input."