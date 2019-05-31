module Test.Chapter14 where

import           Chapter14
import           Data.List       (sort)
import           Test.Hspec
import           Test.QuickCheck

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = arbitrary

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = arbitrary

genTupleNotZero :: (Arbitrary a, Num a, Eq a) => Gen (a, a)
genTupleNotZero = do
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
    it "half of x should work /= 2" $
      property $ \x -> half x * 2 == (x :: Double)
    it "the half identity should hold" $
      property $ \x -> halfIdentity x == (x :: Double)
    it "for any list you apply sort to" $
      property $ \x -> listOrdered $ sort (x :: [Int])
    it "addition should be associative" $
      forAll (genThreeple :: Gen (Int, Int, Int)) (uncurry3 $ associative (+))
    it "addition should be commutative" $
      forAll (genTuple :: Gen (Int, Int)) (uncurry $ commutative (+))
    it "multiplication should be associative" $
      forAll (genThreeple :: Gen (Int, Int, Int)) (uncurry3 $ associative (*))
    it "multiplication should be commutative" $
      forAll (genTuple :: Gen (Int, Int)) (uncurry $ commutative (*))
    it "quot and rem should be related" $
      forAll (genTupleNotZero :: Gen (Int, Int)) (uncurry $ quotAndRem)
    it "div and mod should be related" $
      forAll (genTupleNotZero :: Gen (Int, Int)) (uncurry $ divAndMod)
    it "exponentiation should *not* be associative" $
      expectFailure $
      forAll (gen3Pos :: Gen (Int, Int, Int)) (uncurry3 $ associative (^))
    it "exponentiation should *not* be commutative" $
      expectFailure $
      forAll (gen2Pos :: Gen (Int, Int)) (uncurry $ commutative (^))
    it "reversing a list twice is the identity of the list" $
      property $ \x -> (reverse . reverse) x == id (x :: [Int])
    it "apply operator ($) should work correctly" $
      property $ \x -> id $ x == id (x :: Int)
    it "composition operator (.) should work correctly" $
      property $ \x -> (id . id) x == id (id (x :: Int))
    it "read is the inverse of show" $
      property $ \x -> (read . show) x == (x :: Int)
