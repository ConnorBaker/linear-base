{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Data.Destination (destArrayTests)
import Test.Data.Mutable.Array (mutArrTests)
import Test.Data.Mutable.HashMap (mutHMTests)
import Test.Data.Mutable.Set (mutSetTests)
import Test.Data.Mutable.Vector (mutVecTests)
import Test.Data.Num.Literal (numLiteralTests)
import Test.Data.Polarized (polarizedArrayTests)
import Test.Data.Replicator (replicatorInspectionTests)
import Test.Data.V (vInspectionTests)
import Test.Tasty

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests =
  testGroup
    "All tests"
    [ testGroup
        "Functional tests"
        [ mutArrTests,
          mutVecTests,
          mutHMTests,
          mutSetTests,
          numLiteralTests,
          destArrayTests,
          polarizedArrayTests
        ],
      testGroup
        "Inspection tests"
        [ vInspectionTests,
          replicatorInspectionTests
        ]
    ]
