module Test.Chapter18 where

import           Chapter18
import           Control.Applicative      (liftA2)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes  hiding (bind)

type Types = (Int, Bool, Double) -- this will be used to generate random values!

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = oneof [First <$> arbitrary, Second <$> arbitrary]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

prop_bind :: (Monad m, Eq (m b)) => (a -> m b) -> m a -> Bool
prop_bind f = liftA2 (==) (bind f) (f =<<)

-- tests
spec :: Spec
spec =
  describe "Chapter 18:" $ do
    it "twice when even should work with List monad" $
      twiceWhenEven [1 .. 3] `shouldBe` [1, 4, 4, 9]
    it "custom bind should work like flipped >>=" $
      property (prop_bind (\x -> [x, 1]) :: [Int] -> Bool)
    it "Sum -> monad laws should hold!" $
      quickBatch $ monad (undefined :: Sum Int Types)
