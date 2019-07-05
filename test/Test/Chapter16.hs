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

-- Three
data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

-- Three'
data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Three' x y y

-- Four
data Four a b c d =
  Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four x y z a) = Four x y z (f a)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    a <- arbitrary
    return $ Four x y z a

-- Four'
data Four' a b =
  Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x y z a) = Four' x y z (f a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Four' x x x y

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
