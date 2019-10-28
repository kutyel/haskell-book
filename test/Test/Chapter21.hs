{-# LANGUAGE FlexibleContexts #-}

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

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property), Eq a, Eq (n a), EqProp a) =>
         EqProp (S n a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    frequency
      [ (3, pure Empty)
      , (2, Leaf <$> arbitrary)
      , (1, Node <$> arbitrary <*> arbitrary <*> arbitrary)
      ]

instance Eq a => EqProp (Tree a) where
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
    it "Pair -> should be traversable" $
      quickBatch $ traversable (undefined :: Pair Int Types)
    it "Big -> should be traversable" $
      quickBatch $ traversable (undefined :: Big Int Types)
    it "Bigger -> should be traversable" $
      quickBatch $ traversable (undefined :: Bigger Int Types)
    it "S -> should be traversable" $
      quickBatch $ traversable (undefined :: S [] Types)
    it "Tree -> should be traversable" $
      quickBatch $ traversable (undefined :: Tree Types)
