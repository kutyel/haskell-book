{-# LANGUAGE FlexibleInstances #-}

module Test.Chapter16 where

import           Chapter16
import           Test.Hspec
import           Test.QuickCheck

instance Arbitrary a => Arbitrary (Possibly a) where
  arbitrary = frequency [(1, pure LolNope), (2, Yeppers <$> arbitrary)]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = oneof [First <$> arbitrary, Second <$> arbitrary]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = oneof [Fst <$> arbitrary, Snd <$> arbitrary]

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Company a c b) where
  arbitrary =
    oneof [DeepBlue <$> arbitrary <*> arbitrary, Something <$> arbitrary]

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = oneof [pure Finance, Desk <$> arbitrary, Bloor <$> arbitrary]

instance Arbitrary a => Arbitrary (K a b) where
  arbitrary = K <$> arbitrary

instance Arbitrary b => Arbitrary (Flip K a b) where
  arbitrary = Flip <$> arbitrary

instance Arbitrary b => Arbitrary (EvilGoateeConst a b) where
  arbitrary = GoatyConst <$> arbitrary

instance Arbitrary a => Arbitrary (LiftItOut Identity a) where
  arbitrary = LiftItOut <$> arbitrary

instance Arbitrary a => Arbitrary (Parappa Identity Identity a) where
  arbitrary = DaWrappa <$> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (IgnoreOne Identity Identity a b) where
  arbitrary = IgnoringSomething <$> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Notorious Identity a b c) where
  arbitrary = Notorious <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, pure Nil), (3, Cons <$> arbitrary <*> arbitrary)]

instance Arbitrary a => Arbitrary (GoatLord a) where
  arbitrary =
    frequency
      [ (3, pure NoGoat)
      , (2, OneGoat <$> arbitrary)
      , (1, MoreGoats <$> arbitrary <*> arbitrary <*> arbitrary)
      ]

-- properties
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

-- tests
spec :: Spec
spec = do
  describe "Functors!" $ do
    describe "heavy lifting!" $ do
      it "a should be [2]" $ a `shouldBe` [2]
      it "b should be Just [Hi,lol,Hello,lol]" $
        b `shouldBe` Just ["Hi,lol", "Hellolol"]
      it "c 1 should be -2" $ c 1 `shouldBe` -2
      it "d 0 should be 1[0,1,2,3]" $ d 0 `shouldBe` "1[0,1,2,3]"
      it "e should be 3693" $ e `shouldReturn` 3693
    describe "Two" $ do
      it "functor identity law should hold" $
        property (functorIdentity :: Two String Int -> Bool)
      it "functor compose law should hold" $
        property (functorCompose (+ 1) (* 2) :: Two String Int -> Bool)
    describe "Or" $ do
      it "functor identity law should hold" $
        property (functorIdentity :: Or String Int -> Bool)
      it "functor compose law should hold" $
        property (functorCompose (+ 1) (* 2) :: Or String Int -> Bool)
    describe "Identity" $ do
      it "functor identity law should hold" $
        property (functorIdentity :: Identity Int -> Bool)
      it "functor compose law should hold" $
        property (functorCompose (+ 1) (* 2) :: Identity Int -> Bool)
    describe "Pair" $ do
      it "functor identity law should hold" $
        property (functorIdentity :: Pair Int -> Bool)
      it "functor compose law should hold" $
        property (functorCompose (+ 1) (* 2) :: Pair Int -> Bool)
    describe "Three" $ do
      it "functor identity law should hold" $
        property (functorIdentity :: Three String Int Int -> Bool)
      it "functor compose law should hold" $
        property (functorCompose (+ 1) (* 2) :: Three String Int Int -> Bool)
    describe "Three'" $ do
      it "functor identity law should hold" $
        property (functorIdentity :: Three' String Int -> Bool)
      it "functor compose law should hold" $
        property (functorCompose (+ 1) (* 2) :: Three' String Int -> Bool)
    describe "Four" $ do
      it "functor identity law should hold" $
        property (functorIdentity :: Four Int Int Int Int -> Bool)
      it "functor compose law should hold" $
        property (functorCompose (+ 1) (* 2) :: Four Int Int Int Int -> Bool)
    describe "Four'" $ do
      it "functor identity law should hold" $
        property (functorIdentity :: Four' String Int -> Bool)
      it "functor compose law should hold" $
        property (functorCompose (+ 1) (* 2) :: Four' String Int -> Bool)
    describe "Quant" $ do
      it "functor identity law should hold" $
        property (functorIdentity :: Quant String Int -> Bool)
      it "functor compose law should hold" $
        property (functorCompose (+ 1) (* 2) :: Quant String Int -> Bool)
    describe "K" $ do
      it "functor identity law should hold" $
        property (functorIdentity :: K String Int -> Bool)
      it "functor compose law should hold" $
        property (functorCompose (+ 1) (* 2) :: K String Int -> Bool)
    describe "Flip" $ do
      it "functor identity law should hold" $
        property (functorIdentity :: Flip K String Int -> Bool)
      it "functor compose law should hold" $
        property (functorCompose (+ 1) (* 2) :: Flip K String Int -> Bool)
    describe "EvilGoateeConst" $ do
      it "functor identity law should hold" $
        property (functorIdentity :: EvilGoateeConst Int Int -> Bool)
      it "functor compose law should hold" $
        property (functorCompose (+ 1) (* 2) :: EvilGoateeConst Int Int -> Bool)
    describe "LiftItOut" $ do
      it "functor identity law should hold" $
        property (functorIdentity :: LiftItOut Identity Int -> Bool)
      it "functor compose law should hold" $
        property (functorCompose (+ 1) (* 2) :: LiftItOut Identity Int -> Bool)
    describe "Parappa" $ do
      it "functor identity law should hold" $
        property (functorIdentity :: Parappa Identity Identity Int -> Bool)
      it "functor compose law should hold" $
        property
          (functorCompose (+ 1) (* 2) :: Parappa Identity Identity Int -> Bool)
    describe "IgnoreOne" $ do
      it "functor identity law should hold" $
        property
          (functorIdentity :: IgnoreOne Identity Identity String Int -> Bool)
      it "functor compose law should hold" $
        property
          (functorCompose (+ 1) (* 2) :: IgnoreOne Identity Identity String Int -> Bool)
    describe "Notorious" $ do
      it "functor identity law should hold" $
        property (functorIdentity :: Notorious Identity Int Int Int -> Bool)
      it "functor compose law should hold" $
        property
          (functorCompose (+ 1) (* 2) :: Notorious Identity Int Int Int -> Bool)
    describe "List" $ do
      it "functor identity law should hold" $
        property (functorIdentity :: List Int -> Bool)
      it "functor compose law should hold" $
        property (functorCompose (+ 1) (* 2) :: List Int -> Bool)
    describe "GoatLord" $ do
      it "functor identity law should hold" $
        property (functorIdentity :: GoatLord Int -> Bool)
      it "functor compose law should hold" $
        property (functorCompose (+ 1) (* 2) :: GoatLord Int -> Bool)
    describe "Possibly" $ do
      it "functor identity law should hold" $
        property (functorIdentity :: Possibly Int -> Bool)
      it "functor compose law should hold" $
        property (functorCompose (+ 1) (* 2) :: Possibly Int -> Bool)
    describe "Sum" $ do
      it "functor identity law should hold" $
        property (functorIdentity :: Sum String Int -> Bool)
      it "functor compose law should hold" $
        property (functorCompose (+ 1) (* 2) :: Sum String Int -> Bool)
    describe "Company" $ do
      it "functor identity law should hold" $
        property (functorIdentity :: Company Int Int Int -> Bool)
      it "functor compose law should hold" $
        property (functorCompose (+ 1) (* 2) :: Company Int Int Int -> Bool)
