module Test.Chapter14 where

import           Chapter14
import           Data.List       (sort)
import           Test.Hspec
import           Test.QuickCheck

genTuple :: (Arbitrary a, Num a, Eq a) => Gen (a, a)
genTuple = do
  x <- arbitrary `suchThat` (/= 0)
  y <- arbitrary `suchThat` (/= 0)
  return (x, y)

gen2Pos :: (Arbitrary a, Num a, Ord a) => Gen (a, a)
gen2Pos = do
  x <- arbitrary `suchThat` (> 1)
  y <- arbitrary `suchThat` (> 1)
  return (x, y)

gen3Pos :: (Arbitrary a, Num a, Ord a) => Gen (a, a, a)
gen3Pos = do
  x <- arbitrary `suchThat` (> 1)
  y <- arbitrary `suchThat` (> 1)
  z <- arbitrary `suchThat` (> 1)
  return (x, y, z)

spec :: Spec
spec = do
  describe "Property Testing" $ do
    it "half of n should work for fractional numbers" $
      property (prop_half :: Double -> Bool)
    it "the half identity should hold" $
      property (prop_halfIdentity :: Double -> Bool)
    it "for any list you apply sort to" $
      property ((listOrdered . sort) :: [Int] -> Bool)
    it "addition should be associative" $
      property (associative (+) :: Int -> Int -> Int -> Bool)
    it "addition should be commutative" $
      property (commutative (+) :: Int -> Int -> Bool)
    it "multiplication should be associative" $
      property (associative (*) :: Int -> Int -> Int -> Bool)
    it "multiplication should be commutative" $
      property (commutative (*) :: Int -> Int -> Bool)
    it "quot and rem should be related" $
      forAll (genTuple :: Gen (Int, Int)) (uncurry $ quotAndRem)
    it "div and mod should be related" $
      forAll (genTuple :: Gen (Int, Int)) (uncurry $ divAndMod)
    it "exponentiation should *not* be associative" $
      expectFailure $
      forAll (gen3Pos :: Gen (Int, Int, Int)) (uncurry3 $ associative (^))
    it "exponentiation should *not* be commutative" $
      expectFailure $
      forAll (gen2Pos :: Gen (Int, Int)) (uncurry $ commutative (^))
    it "reversing a list twice is the identity of the list" $
      property (prop_reverseTwice :: [Int] -> Bool)
    it "apply operator ($) should work correctly" $
      property (prop_applyOperator :: Int -> Bool)
    it "composition operator (.) should work correctly" $
      property (prop_composition :: String -> Bool)
    it "read is the inverse of show" $
      property (prop_roundTrip :: String -> Bool)
