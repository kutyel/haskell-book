module Test.Chapter16 where

import           Control.Monad   (liftM)
import           Test.Hspec
import           Test.QuickCheck

-- Two
data Two a b =
  Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

-- Or
data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance Functor (Or a) where
  fmap _ (Fst x) = Fst x
  fmap f (Snd x) = Snd (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [Fst x, Snd y]

-- Identity
data Identity a =
  Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = liftM Identity arbitrary

-- Pair
data Pair a =
  Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    return $ Pair x x

-- Trivial can't be made into a Functor because -> :k Trivial = * != * -> *
data Trivial =
  Trivial

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
