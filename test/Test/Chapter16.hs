module Test.Chapter16 where

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

-- Trivial can't be made into a Functor because -> :k Trivial = * != * -> *
-- properties
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

-- type aliases
type TwoStrInt = Two String Int

type OrStrInt = Or String Int

-- tests
spec :: Spec
spec = do
  describe "Functors!" $ do
    describe "Two" $ do
      it "functor identity law should hold" $
        property (functorIdentity :: TwoStrInt -> Bool)
      it "functor compose law should hold" $
        property (functorCompose (+ 1) (* 2) :: TwoStrInt -> Bool)
    describe "Or" $ do
      it "functor identity law should hold" $
        property (functorIdentity :: OrStrInt -> Bool)
      it "functor compose law should hold" $
        property (functorCompose (+ 1) (* 2) :: OrStrInt -> Bool)
