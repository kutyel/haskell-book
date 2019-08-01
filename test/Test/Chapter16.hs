{-# LANGUAGE FlexibleInstances #-}

module Test.Chapter16 where

import           Chapter16
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

type Types = (Int, Bool, Double)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = oneof [Fst <$> arbitrary, Snd <$> arbitrary]

instance (Eq a, Eq b) => EqProp (Or a b) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (More b a) where
  arbitrary =
    oneof
      [ L <$> arbitrary <*> arbitrary <*> arbitrary
      , R <$> arbitrary <*> arbitrary <*> arbitrary
      ]

instance (Eq a, Eq b) => EqProp (More b a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = oneof [First <$> arbitrary, Second <$> arbitrary]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum' a b) where
  arbitrary = oneof [First' <$> arbitrary, Second' <$> arbitrary]

instance (Eq a, Eq b) => EqProp (Sum' a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Company a c b) where
  arbitrary =
    oneof [DeepBlue <$> arbitrary <*> arbitrary, Something <$> arbitrary]

instance (Eq a, Eq b, Eq c) => EqProp (Company a c b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = oneof [pure Finance, Desk <$> arbitrary, Bloor <$> arbitrary]

instance (Eq a, Eq b) => EqProp (Quant a b) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (K a b) where
  arbitrary = K <$> arbitrary

instance Eq a => EqProp (K a b) where
  (=-=) = eq

instance Arbitrary b => Arbitrary (Flip K a b) where
  arbitrary = Flip <$> arbitrary

instance Eq b => EqProp (Flip K a b) where
  (=-=) = eq

instance Arbitrary b => Arbitrary (EvilGoateeConst a b) where
  arbitrary = GoatyConst <$> arbitrary

instance Eq b => EqProp (EvilGoateeConst a b) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (LiftItOut Identity a) where
  arbitrary = LiftItOut <$> arbitrary

instance Eq a => EqProp (LiftItOut Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Parappa Identity Identity a) where
  arbitrary = DaWrappa <$> arbitrary <*> arbitrary

instance Eq a => EqProp (Parappa Identity Identity a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (IgnoreOne Identity Identity a b) where
  arbitrary = IgnoringSomething <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (IgnoreOne Identity Identity a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Notorious Identity a b c) where
  arbitrary = Notorious <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Notorious Identity a b c) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, pure Nil), (3, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Possibly a) where
  arbitrary = frequency [(1, pure LolNope), (2, Yeppers <$> arbitrary)]

instance Eq a => EqProp (Possibly a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (GoatLord a) where
  arbitrary =
    frequency
      [ (3, pure NoGoat)
      , (2, OneGoat <$> arbitrary)
      , (1, MoreGoats <$> arbitrary <*> arbitrary <*> arbitrary)
      ]

instance Eq a => EqProp (GoatLord a) where
  (=-=) = eq

-- tests
spec :: Spec
spec =
  describe "Chapter 16:" $ do
    it "a should be [2]" $ a `shouldBe` [2]
    it "b should be Just [Hi,lol,Hello,lol]" $
      b `shouldBe` Just ["Hi,lol", "Hellolol"]
    it "c 1 should be -2" $ c 1 `shouldBe` -2
    it "d 0 should be 1[0,1,2,3]" $ d 0 `shouldBe` "1[0,1,2,3]"
    it "e should be 3693" $ e `shouldReturn` 3693
    it "Two -> functor laws should hold!" $
      quickBatch $ functor (undefined :: Two String Types)
    it "Or -> functor laws should hold!" $
      quickBatch $ functor (undefined :: Or String Types)
    it "Identity -> functor laws should hold!" $
      quickBatch $ functor (undefined :: Identity Types)
    it "Pair -> functor laws should hold!" $
      quickBatch $ functor (undefined :: Pair Types)
    it "Three -> functor laws should hold!" $
      quickBatch $ functor (undefined :: Three String Int Types)
    it "Three' -> functor laws should hold!" $
      quickBatch $ functor (undefined :: Three' Int Types)
    it "Four -> functor laws should hold!" $
      quickBatch $ functor (undefined :: Four String Int Int Types)
    it "Four' -> functor laws should hold!" $
      quickBatch $ functor (undefined :: Four' Int Types)
    it "More -> functor laws should hold!" $
      quickBatch $ functor (undefined :: More Int Types)
    it "Quant -> functor laws should hold!" $
      quickBatch $ functor (undefined :: Quant Int Types)
    it "K -> functor laws should hold!" $
      quickBatch $ functor (undefined :: K Int Types)
    it "Flip -> functor laws should hold!" $
      quickBatch $ functor (undefined :: Flip K Int Types)
    it "EvilGoateeConst -> functor laws should hold!" $
      quickBatch $ functor (undefined :: EvilGoateeConst Int Types)
    it "LiftItOut -> functor laws should hold!" $
      quickBatch $ functor (undefined :: LiftItOut Identity Types)
    it "Parappa -> functor laws should hold!" $
      quickBatch $ functor (undefined :: Parappa Identity Identity Types)
    it "IgnoreOne -> functor laws should hold!" $
      quickBatch $ functor (undefined :: IgnoreOne Identity Identity Int Types)
    it "Notorious -> functor laws should hold!" $
      quickBatch $ functor (undefined :: Notorious Identity Int Int Types)
    it "List -> functor laws should hold!" $
      quickBatch $ functor (undefined :: List Types)
    it "GoatLord -> functor laws should hold!" $
      quickBatch $ functor (undefined :: GoatLord Types)
    it "Possibly -> functor laws should hold!" $
      quickBatch $ functor (undefined :: Possibly Types)
    it "Sum -> functor laws should hold!" $
      quickBatch $ functor (undefined :: Sum Int Types)
    it "Sum' -> functor laws should hold!" $
      quickBatch $ functor (undefined :: Sum' Int Types)
    it "Company -> functor laws should hold!" $
      quickBatch $ functor (undefined :: Company Int Int Types)
