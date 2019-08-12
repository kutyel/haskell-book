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

instance Arbitrary (Nope a) where
  arbitrary = pure NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = oneof [PLeft <$> arbitrary, PRight <$> arbitrary]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, pure Nil), (3, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

-- tests
spec :: Spec
spec =
  describe "Chapter 18:" $ do
    it "twice when even should work with List monad" $
      twiceWhenEven [1 .. 3] `shouldBe` [1, 4, 4, 9]
    it "custom bind should work like flipped >>=" $
      property (prop_bind (\x -> [x, 1]) :: [Int] -> Bool)
    it "Sum -> monad laws should hold!" $ do
      quickBatch $ applicative (undefined :: Sum Int Types)
      quickBatch $ monad (undefined :: Sum Int Types)
    it "Nope -> monad laws should hold!" $ do
      quickBatch $ applicative (undefined :: Nope Types)
      quickBatch $ monad (undefined :: Nope Types)
    it "BahEither -> monad laws should hold!" $ do
      quickBatch $ applicative (undefined :: BahEither Int Types)
      quickBatch $ monad (undefined :: BahEither Int Types)
    it "Identity -> monad laws should hold!" $ do
      quickBatch $ applicative (undefined :: Identity Types)
      quickBatch $ monad (undefined :: Identity Types)
    it "List -> monad laws should hold!" $ do
      quickBatch $ applicative (undefined :: List Types)
      quickBatch $ monad (undefined :: List Types)
