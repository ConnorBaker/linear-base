{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
-- Tests for mutable hashmaps
-- We are excluding tests for alter because it's assumed that alter is
-- implemented parametrically around insert and delete.
module Test.Data.Mutable.HashMap
  ( mutHMTests,
  )
where

import qualified Data.Functor.Linear as Linear
import qualified Data.HashMap.Linear as HashMap
import Data.Unrestricted.Linear
import Data.Function ((&))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Prelude.Linear as Linear
import qualified Data.Map.Lazy as Map
import Data.List (sort)
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)

-- # Exported Tests
--------------------------------------------------------------------------------

mutHMTests :: TestTree
mutHMTests = testGroup "Mutable hashmap tests" group

group :: [TestTree]
group =
  [ testProperty "∀ k,v,m. lookup k (insert m k v) = Just v" lookupInsert1,
    testProperty
      "∀ k,v,m,k'/=k. lookup k'(insert m k v) = lookup k' m"
      lookupInsert2,
    testProperty "∀ k,m. lookup k (delete m k) = Nothing" lookupDelete1,
    testProperty
      "∀ k,m,k'/=k. lookup k' (delete m k) = lookup k' m"
      lookupDelete2,
    testProperty "∀ k,v,m. member k (insert m k v) = True" memberInsert,
    testProperty "∀ k,m. member k (delete m k) = False" memberDelete,
    testProperty "∀ k,v,m. size (insert (m-k) k v) = 1+ size (m-k)" sizeInsert,
    testProperty "∀ k,m with k. size (delete m k) + 1 = size m" deleteSize,
    testProperty "toList . fromList = id" refToListFromList,
    testProperty "filter f (fromList xs) = fromList (filter f xs)" refFilter,
    testProperty "fromList xs <> fromList ys = fromList (xs <> ys)" refMappend,
    testProperty "unionWith reference" refUnionWith,
    testProperty "intersectionWith reference" refIntersectionWith
  ]

-- # Internal Library
--------------------------------------------------------------------------------

-- # Mini Testing Framework
----------------------------------------

-- | All tests are on maps from int to string
type HMap = HashMap.HashMap Int String

-- | A test checks a boolean property on a hashmap and consumes it
type HMTest = HMap #-> Ur Bool


maxSize :: Int
maxSize = 800

-- HashMap's have lots of corner cases, so we try harder to find them.
defProperty :: PropertyT IO () -> Property
defProperty = withTests 1000 . property

-- | Run a test on a random HashMap
testOnAnyHM :: PropertyT IO HMTest -> Property
testOnAnyHM propHmtest = defProperty $ do
  kvs <- forAll keyVals
  hmtest <- propHmtest
  assert $ unur Linear.$ HashMap.fromList kvs hmtest

testKVPairExists :: (Int, String) -> HMTest
testKVPairExists (k, v) hmap =
  fromLookup Linear.$ getFst Linear.$ HashMap.lookup hmap k
  where
    fromLookup :: Ur (Maybe String) #-> Ur Bool
    fromLookup (Ur Nothing) = Ur False
    fromLookup (Ur (Just v')) = Ur (v' == v)

testKeyMember :: Int -> HMTest
testKeyMember key hmap = getFst Linear.$ HashMap.member hmap key

testKeyNotMember :: Int -> HMTest
testKeyNotMember key hmap = Linear.fmap Linear.not (testKeyMember key hmap)

-- | That is, test that lookup gives us `Nothing`
testKeyMissing :: Int -> HMTest
testKeyMissing key hmap =
  fromLookup Linear.$ getFst Linear.$ HashMap.lookup hmap key
  where
    fromLookup :: Ur (Maybe String) #-> Ur Bool
    fromLookup (Ur Nothing) = Ur True
    fromLookup (Ur _) = Ur False

testLookupUnchanged :: (HMap #-> HMap) -> Int -> HMTest
testLookupUnchanged f k hmap = fromLookup (HashMap.lookup hmap k)
  where
    fromLookup :: (Ur (Maybe String), HMap) #-> Ur Bool
    fromLookup (look1, hmap') =
      compareMaybes look1 (getFst Linear.$ HashMap.lookup (f hmap') k)

deleteKey :: Int -> HMap #-> HMap
deleteKey key hmap = HashMap.delete hmap key

insertPair :: (Int, String) -> HMap #-> HMap
insertPair (k, v) hmap = HashMap.insert hmap k v

-- XXX: This is a terrible name
getFst :: (Consumable b) => (a, b) #-> a
getFst (a, b) = lseq b a

compareMaybes :: Eq a =>
  Ur (Maybe a) #->
  Ur (Maybe a) #->
  Ur Bool
compareMaybes (Ur a) (Ur b) = Ur (a == b)

-- # Random Generation
----------------------------------------

-- | Key generator
key :: Gen Int
key = Gen.int $ Range.linearFrom 0 (-20) 20

-- | Value generator
val :: Gen String
val = do
  let strSize = Range.singleton 3
  Gen.string strSize Gen.alpha

-- | Random pairs
keyVals :: Gen [(Int, String)]
keyVals = do
  size <- Gen.int $ Range.linear 0 maxSize
  let sizeGen = Range.singleton size
  keys <- Gen.list sizeGen key
  vals <- Gen.list sizeGen val
  return $ zip keys vals

-- # Tests
--------------------------------------------------------------------------------

lookupInsert1 :: Property
lookupInsert1 = testOnAnyHM $ do
  k <- forAll key
  v <- forAll val
  let insertKV = insertPair (k, v)
  let checkKV = testKVPairExists (k, v)
  return (checkKV Linear.. insertKV)

lookupInsert2 :: Property
lookupInsert2 = testOnAnyHM $ do
  k <- forAll key
  k' <- forAll $ Gen.filter (/= k) key
  v <- forAll val
  let insertKV = insertPair (k, v)
  return (testLookupUnchanged insertKV k')

lookupDelete1 :: Property
lookupDelete1 = testOnAnyHM $ do
  k <- forAll key
  let deleteK = deleteKey k
  let checkNoKey = testKeyMissing k
  return (checkNoKey Linear.. deleteK)

lookupDelete2 :: Property
lookupDelete2 = testOnAnyHM $ do
  k <- forAll key
  k' <- forAll $ Gen.filter (/= k) key
  let deleteK = deleteKey k
  return (testLookupUnchanged deleteK k')

memberInsert :: Property
memberInsert = testOnAnyHM $ do
  k <- forAll key
  v <- forAll val
  let insertKV = insertPair (k, v)
  let isMemberK = testKeyMember k
  return (isMemberK Linear.. insertKV)

memberDelete :: Property
memberDelete = testOnAnyHM $ do
  k <- forAll key
  v <- forAll val
  let pair = (k, v)
  let deleteK = deleteKey k
  let insertKV = insertPair pair
  let checkNotMember = testKeyNotMember k
  return (checkNotMember Linear.. deleteK Linear.. insertKV)

sizeInsert :: Property
sizeInsert = testOnAnyHM $ do
  k <- forAll key
  v <- forAll val
  let pair = (k, v)
  let deleteK = deleteKey k
  let insertCheckSize = checkSizeAfterInsert pair
  return (insertCheckSize Linear.. deleteK)

checkSizeAfterInsert :: (Int, String) -> HMTest
checkSizeAfterInsert (k, v) hmap = withSize Linear.$ HashMap.size hmap
  where
    withSize :: (Ur Int, HMap) #-> Ur Bool
    withSize (oldSize, hmap) =
      checkSize oldSize
        Linear.$ getFst
        Linear.$ HashMap.size
        Linear.$ HashMap.insert hmap k v
    checkSize :: Ur Int #-> Ur Int #-> Ur Bool
    checkSize (Ur old) (Ur new) =
      Ur ((old + 1) == new)

deleteSize :: Property
deleteSize = testOnAnyHM $ do
  k <- forAll key
  v <- forAll val
  let insertKV = insertPair (k, v)
  let checkSize = checkSizeAfterDelete k
  return (checkSize Linear.. insertKV)

checkSizeAfterDelete :: Int -> HMTest
checkSizeAfterDelete key hmap = fromSize (HashMap.size hmap)
  where
    fromSize :: (Ur Int, HMap) #-> Ur Bool
    fromSize (orgSize, hmap) =
      compSizes orgSize
        Linear.$ getFst
        Linear.$ HashMap.size (HashMap.delete hmap key)
    compSizes :: Ur Int #-> Ur Int #-> Ur Bool
    compSizes (Ur orgSize) (Ur newSize) =
      Ur ((newSize + 1) == orgSize)

refToListFromList :: Property
refToListFromList = defProperty $ do
  xs <- forAll keyVals

  let expected = Map.fromList xs
                   & Map.toList

      Ur actual = HashMap.fromList xs HashMap.toList

  sort expected === sort actual

refFilter :: Property
refFilter = defProperty $ do
  xs <- forAll keyVals

  let predicate "" = False
      predicate (i:_) = i < 'h'

      expected = Map.fromList xs
                   & Map.filter predicate
                   & Map.toList

      Ur actual = HashMap.fromList xs Linear.$
        HashMap.toList Linear.. HashMap.filter predicate

  sort expected === sort actual

refMappend :: Property
refMappend = defProperty $ do
  xs <- forAll keyVals
  ys <- forAll keyVals

  let Ur expected =
        HashMap.fromList (xs <> ys) Linear.$ HashMap.toList

      Ur actual =
        HashMap.fromList xs Linear.$ \hx ->
          HashMap.fromList ys Linear.$ \hy ->
            HashMap.toList (hx Linear.<> hy)

  sort expected === sort actual

refUnionWith :: Property
refUnionWith = defProperty $ do
  xs <- forAll keyVals
  ys <- forAll keyVals

  let combine a b = a ++ "," ++ b

      expected = Map.unionWith combine
                  (Map.fromList xs)
                  (Map.fromList ys)
                  & Map.toList

      Ur actual =
        HashMap.fromList xs Linear.$ \hx ->
          HashMap.fromList ys Linear.$ \hy ->
            HashMap.unionWith combine hx hy
              Linear.& HashMap.toList

  sort expected === sort actual

refIntersectionWith :: Property
refIntersectionWith = defProperty $ do
  xs <- forAll keyVals
  ys <- forAll keyVals

  let expected = Map.intersectionWith (,)
                  (Map.fromList xs)
                  (Map.fromList ys)
                  & Map.toList

      Ur actual =
        HashMap.fromList xs Linear.$ \hx ->
          HashMap.fromList ys Linear.$ \hy ->
            HashMap.intersectionWith (,) hx hy
              Linear.& HashMap.toList

  sort expected === sort actual
