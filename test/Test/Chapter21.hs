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

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, pure Nil), (3, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
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
    it "List -> should be traversable" $
      quickBatch $ traversable (undefined :: List Types)
    it "Three -> should be traversable" $
      quickBatch $ traversable (undefined :: Three Int Bool Types)
