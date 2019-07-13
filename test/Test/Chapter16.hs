{-# LANGUAGE FlexibleInstances #-}

module Test.Chapter16 where

import           Chapter16
import           Control.Monad   (liftM)
import           Test.Hspec
import           Test.QuickCheck

instance Arbitrary a => Arbitrary (Possibly a) where
  arbitrary = do
    x <- arbitrary
    frequency [(1, return LolNope), (2, return $ Yeppers x)]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [Fst x, Snd y]

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = liftM Identity arbitrary

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    return $ Pair x x

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Three' x y y

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    a <- arbitrary
    return $ Four x y z a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Four' x x x y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [Finance, Desk x, Bloor y]

instance Arbitrary a => Arbitrary (K a b) where
  arbitrary = liftM K arbitrary

instance Arbitrary b => Arbitrary (Flip K a b) where
  arbitrary = liftM (Flip . K) arbitrary

instance Arbitrary b => Arbitrary (EvilGoateeConst a b) where
  arbitrary = liftM GoatyConst arbitrary

instance Arbitrary a => Arbitrary (LiftItOut Identity a) where
  arbitrary = liftM (LiftItOut . Identity) arbitrary

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, pure Nil), (3, Cons <$> arbitrary <*> arbitrary)]

instance Arbitrary a => Arbitrary (GoatLord a) where
  arbitrary = do
    x <- arbitrary
    frequency
      [ (1, return NoGoat)
      , (2, return $ OneGoat x)
      , (2, return $ MoreGoats NoGoat (OneGoat x) (OneGoat x))
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
