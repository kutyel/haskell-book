module Test.Chapter14 where

import           Chapter14
import           Data.List       (sort)
import           Test.Hspec
import           Test.QuickCheck

data Fool
  = Fulse
  | Frue
  deriving (Eq, Show)

genFoolEqual :: Gen Fool
genFoolEqual = elements [Fulse, Frue]

genFool :: Gen Fool
genFool = frequency [(3, return Fulse), (1, return Frue)]

prop_quotAndRem :: Integral a => NonZero a -> NonZero a -> Bool
prop_quotAndRem (NonZero x) (NonZero y) = (quot x y) * y + (rem x y) == x

prop_divAndMod :: Integral a => NonZero a -> NonZero a -> Bool
prop_divAndMod (NonZero x) (NonZero y) = (div x y) * y + (mod x y) == x

prop_squareId :: (Eq a, Floating a) => a -> Bool
prop_squareId x = squareIdentity x == x

assocExp :: Integral a => Positive a -> Positive a -> Positive a -> Bool
assocExp (Positive x) (Positive y) (Positive z) = associative (^) x y z

commuExp :: Integral a => Positive a -> Positive a -> Bool
commuExp (Positive x) (Positive y) = commutative (^) x y

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
      property (prop_quotAndRem :: NonZero Int -> NonZero Int -> Bool)
    it "div and mod should be related" $
      property (prop_divAndMod :: NonZero Int -> NonZero Int -> Bool)
    it "exponentiation should *not* be associative" $
      expectFailure $
      property
        (assocExp :: Positive Int -> Positive Int -> Positive Int -> Bool)
    it "exponentiation should *not* be commutative" $
      expectFailure $
      property (commuExp :: Positive Int -> Positive Int -> Bool)
    it "reversing a list twice is the identity of the list" $
      property (prop_reverseTwice :: [Int] -> Bool)
    it "apply operator ($) should work correctly" $
      property (prop_applyOperator :: Int -> Bool)
    it "composition operator (.) should work correctly" $
      property (prop_composition :: String -> Bool)
    it "read is the inverse of show" $
      property (prop_roundTrip :: String -> Bool)
    it "folding by cons shoud *not* concat" $
      expectFailure $ property (prop_foldrPlusPlus :: [Int] -> [Int] -> Bool)
    it "folding by concat with empty list should equal concat" $
      property (prop_foldrConcat :: [[Int]] -> Bool)
    it "take and length of n should not hold" $
      expectFailure $ property (prop_takeLength :: Int -> [Int] -> Bool)
    it "floating point arithmetic should fail -.-" $
      expectFailure $ property (prop_squareId :: Double -> Bool)
    it "idempotence should work for capitalizing" $
      property (prop_idemCapitalize :: String -> Bool)
    it "idempotence should work for sorting" $
      property (prop_idemSort :: [Int] -> Bool)
