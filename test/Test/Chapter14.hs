module Test.Chapter14 where

import           Test.Hspec
import           Test.QuickCheck

divisor :: Gen Double
divisor = arbitrary `suchThat` (/= 0)

half :: Fractional a => a -> a
half = (/ 2)

prop_half :: Property
prop_half = forAll divisor (\x -> half x * 2 == x)

halfIdentity :: Fractional a => a -> a
halfIdentity = (* 2) . half

prop_halfIdentity :: Property
prop_halfIdentity = forAll divisor (\x -> halfIdentity x == x)

spec :: Spec
spec = do
  describe "Property Testing" $ do
    it "half of x should work" $ do quickCheck prop_half
    it "the half identity should hold" $ do quickCheck prop_halfIdentity
