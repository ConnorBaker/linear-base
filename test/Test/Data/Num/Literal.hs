{-# LANGUAGE NoImplicitPrelude #-}

module Test.Data.Num.Literal where

import Prelude.Linear
import Test.Tasty
import Test.Tasty.HUnit

-- These tests are here to ensure that the numerical literals are
-- correctly interpreted as the correct type.
numLiteralTests :: TestTree
numLiteralTests =
  testGroup
    "Numerical literal tests"
    [ testCase "testOp Float" $ (4 :: Float) @=? (testOp 1 1),
      testCase "testOp Double" $ (4 :: Double) @=? (testOp 1 1)
    ]

testOp :: (Floating a) => a -> a -> a
testOp x y = (x + y) ** 2
