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

prob3    :: PExp -> Result a b
prob3    = undefined

prob4    :: PExp -> String
prob4    = undefined

-- Write your Hspec Tests below

test_probs :: IO()
test_probs = do
  test_prob1
  test_prob2
  test_prob3
  test_prob4

test_prob1 :: IO()
test_prob1 = undefined

test_prob2 :: IO()
test_prob2 = undefined

test_prob3 :: IO()
test_prob3 = undefined

test_prob4 :: IO()
test_prob4 = undefined