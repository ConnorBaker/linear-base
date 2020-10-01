{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
-- Tests for mutable sets.

module Test.Data.Mutable.Set
  ( mutSetTests,
  )
where

import qualified Data.Set.Mutable.Linear as Set
import Data.Unrestricted.Linear
import Hedgehog
import qualified Data.Functor.Linear as Data
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Prelude.Linear as Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- # Exported Tests
--------------------------------------------------------------------------------

mutSetTests :: TestTree
mutSetTests = testGroup "Mutable set tests" group

group :: [TestTree]
group =
  -- Tests of the form [accessor (mutator)]
  [ testProperty "∀ x. member (insert s x) x = True" memberInsert1
  , testProperty "∀ x,y/=x. member (insert s x) y = member s y" memberInsert2
  , testProperty "∀ x. member (delete s x) x = False" memberDelete1
  , testProperty "∀ x,y/=x. member (delete s x) y = member s y" memberDelete2
  , testProperty "∀ s, x \\in s. size (insert s x) = size s" sizeInsert1
  , testProperty "∀ s, x \\notin s. size (insert s x) = size s + 1" sizeInsert2
  , testProperty "∀ s, x \\in s. size (delete s x) = size s - 1" sizeDelete1
  , testProperty "∀ s, x \\notin s. size (delete s x) = size s" sizeDelete2
  ]

-- # Internal Library
--------------------------------------------------------------------------------

type SetTester = Set.Set Int #-> Ur (TestT IO ())

-- | A random list
nonemptyList :: Gen [Int]
nonemptyList = do
  size <- Gen.int $ Range.linearFrom 0 0 1000
  let size' = Range.singleton size
  Gen.list size' $ Gen.int $ Range.linearFrom 0 (-100) 100

-- | A random value
value :: Gen Int
value = Gen.int (Range.linear (-100) 100)

testEqual :: (Show a, Eq a) =>
  Ur a #->
  Ur a #->
  Ur (TestT IO ())
testEqual (Ur x) (Ur y) = Ur (x === y)

-- XXX: This is a terrible name
getFst :: Consumable b => (a, b) #-> a
getFst (a, b) = lseq b a

-- # Tests
--------------------------------------------------------------------------------

memberInsert1 :: Property
memberInsert1 = property $ do
  val <- forAll value
  l <- forAll nonemptyList
  let tester = memberInsert1Test val
  test $ unur Linear.$ Set.fromList l tester

memberInsert1Test :: Int -> SetTester
memberInsert1Test val set =
  testEqual
    (Ur True)
    (getFst (Set.member val (Set.insert val set)))

memberInsert2 :: Property
memberInsert2 = property $ do
  val1 <- forAll value
  val2 <- forAll $ Gen.filter (/= val1) value
  l <- forAll nonemptyList
  let tester = memberInsert2Test val1 val2
  test $ unur Linear.$ Set.fromList l tester

memberInsert2Test :: Int -> Int -> SetTester
memberInsert2Test val1 val2 set = fromRead (Set.member val2 set)
  where
    fromRead :: (Ur Bool, Set.Set Int) #-> Ur (TestT IO ())
    fromRead (memberVal2, set) =
      testEqual
        memberVal2
        (getFst (Set.member val2 (Set.insert val1 set)))

memberDelete1 :: Property
memberDelete1 = property $ do
  val <- forAll value
  l <- forAll nonemptyList
  let tester = memberDelete1Test val
  test $ unur Linear.$ Set.fromList l tester

memberDelete1Test :: Int -> SetTester
memberDelete1Test val set =
  testEqual
    (Ur False)
    (getFst (Set.member val (Set.delete val set)))

memberDelete2 :: Property
memberDelete2 = property $ do
  val1 <- forAll value
  val2 <- forAll $ Gen.filter (/= val1) value
  l <- forAll nonemptyList
  let tester = memberDelete2Test val1 val2
  test $ unur Linear.$ Set.fromList l tester

memberDelete2Test :: Int -> Int -> SetTester
memberDelete2Test val1 val2 set = fromRead (Set.member val2 set)
  where
    fromRead :: (Ur Bool, Set.Set Int) #-> Ur (TestT IO ())
    fromRead (memberVal2, set) =
      testEqual
        memberVal2
        (getFst Linear.$ Set.member val2 (Set.delete val1 set))

sizeInsert1 :: Property
sizeInsert1 = property $ do
  l <- forAll nonemptyList
  val <- forAll $ Gen.filter (`elem` l) value
  let tester = sizeInsert1Test val
  test $ unur Linear.$ Set.fromList l tester

sizeInsert1Test :: Int -> SetTester
sizeInsert1Test val set = fromRead (Set.size set)
  where
    fromRead :: (Ur Int, Set.Set Int) #-> Ur (TestT IO ())
    fromRead (sizeOriginal, set) =
      testEqual
        sizeOriginal
        (getFst Linear.$ (Set.size (Set.insert val set)))

sizeInsert2 :: Property
sizeInsert2 = property $ do
  l <- forAll nonemptyList
  val <- forAll $ Gen.filter (not . (`elem` l)) value
  let tester = sizeInsert2Test val
  test $ unur Linear.$ Set.fromList l tester

sizeInsert2Test :: Int -> SetTester
sizeInsert2Test val set = fromRead (Set.size set)
  where
    fromRead :: (Ur Int, Set.Set Int) #-> Ur (TestT IO ())
    fromRead (sizeOriginal, set) =
      testEqual
        ((Linear.+ 1) Data.<$> sizeOriginal)
        (getFst Linear.$ (Set.size (Set.insert val set)))

sizeDelete1 :: Property
sizeDelete1 = property $ do
  l <- forAll nonemptyList
  val <- forAll $ Gen.filter (`elem` l) value
  let tester = sizeDelete1Test val
  test $ unur Linear.$ Set.fromList l tester

sizeDelete1Test :: Int -> SetTester
sizeDelete1Test val set = fromRead (Set.size set)
  where
    fromRead :: (Ur Int, Set.Set Int) #-> Ur (TestT IO ())
    fromRead (sizeOriginal, set) =
      testEqual
        ((Linear.- 1) Data.<$> sizeOriginal)
        (getFst Linear.$ (Set.size (Set.delete val set)))

sizeDelete2 :: Property
sizeDelete2 = property $ do
  l <- forAll nonemptyList
  val <- forAll $ Gen.filter (not . (`elem` l)) value
  let tester = sizeDelete2Test val
  test $ unur Linear.$ Set.fromList l tester

sizeDelete2Test :: Int -> SetTester
sizeDelete2Test val set = fromRead (Set.size set)
  where
    fromRead :: (Ur Int, Set.Set Int) #-> Ur (TestT IO ())
    fromRead (sizeOriginal, set) =
      testEqual
        sizeOriginal
        (getFst Linear.$ (Set.size (Set.delete val set)))
