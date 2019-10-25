module Test.Chapter21 where

import           Chapter21

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

type Types = (Int, Bool, String)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, pure Nada), (3, Yep <$> arbitrary)]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

spec :: Spec
spec =
  describe "Chapter 20:" $ do
    it "Identity -> should be traversable" $
      quickBatch $ traversable (undefined :: Identity Types)
    it "Constant -> should be traversable" $
      quickBatch $ traversable (undefined :: Constant String Types)
    it "Optional -> should be traversable" $
      quickBatch $ traversable (undefined :: Optional Types)
