module Test.Chapter20Spec where

import Chapter20
import Control.Applicative (liftA2)
import qualified Data.Foldable as F
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..))
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

type Types = (Int, Bool, String, Int, Bool)

prop_eq_fn :: (Applicative f, Eq a) => f a -> f a -> f Bool
prop_eq_fn = liftA2 (==)

instance Arbitrary b => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq b => EqProp (Constant a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance
  ( Arbitrary a,
    Arbitrary b,
    Arbitrary c
  ) =>
  Arbitrary (Three a b c)
  where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

spec :: Spec
spec =
  describe "Chapter 20:" $ do
    it "custom sum should work like the one in Prelude" $
      property (prop_eq_fn Prelude.sum Chapter20.sum :: [Int] -> Bool)
    it "custom product should work like the one in Prelude" $
      property (prop_eq_fn Prelude.product Chapter20.product :: [Int] -> Bool)
    it "custom elem should work like the one in Prelude" $
      property (prop_eq_fn (Prelude.elem 1) (Chapter20.elem 1) :: [Int] -> Bool)
    it "custom minimum should work like the one in Prelude" $
      property
        (prop_eq_fn Prelude.minimum (fromMaybe 0 . Chapter20.minimum) :: Constant String Int -> Bool)
    it "custom maximum should work like the one in Prelude" $
      property
        (prop_eq_fn Prelude.maximum (fromMaybe 0 . Chapter20.maximum) :: Constant String Int -> Bool)
    it "custom null should work like the one in Prelude" $
      property (prop_eq_fn Prelude.null Chapter20.null :: [Int] -> Bool)
    it "custom length should work like the one in Prelude" $
      property (prop_eq_fn Prelude.length Chapter20.length :: [Int] -> Bool)
    it "custom toList should work like the one in Prelude" $
      property (prop_eq_fn F.toList toList :: [Int] -> Bool)
    it "custom fold should work like the one in Prelude" $
      property (prop_eq_fn F.fold fold :: [String] -> Bool)
    it "custom foldMap should work like the one in Prelude" $
      property (prop_eq_fn (foldMap Sum) (foldMap' Sum) :: [Int] -> Bool)
    it "Constant -> all foldable functions should be derivable"
      $ quickBatch
      $ foldable (undefined :: Constant String Types)
    it "Two -> all foldable functions should be derivable"
      $ quickBatch
      $ foldable (undefined :: Two String Types)
    it "Three -> all foldable functions should be derivable"
      $ quickBatch
      $ foldable (undefined :: Three String String Types)
    it "Three' -> all foldable functions should be derivable"
      $ quickBatch
      $ foldable (undefined :: Three' String Types)
    it "Four' -> all foldable functions should be derivable"
      $ quickBatch
      $ foldable (undefined :: Four' String Types)
