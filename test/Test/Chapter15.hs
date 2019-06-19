module Test.Chapter15 where

import           Chapter15       (Optional (..))
import           Control.Monad   (liftM)
import           Data.Monoid
import           Test.Hspec
import           Test.QuickCheck

-- Bull
data Bull
  = Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = elements [Fools, Twoo]

instance Semigroup Bull where
  _ <> _ = Fools

instance Monoid Bull where
  mempty = Fools

-- First'
newtype First' a =
  First'
    { getFirst' :: Optional a
    }
  deriving (Eq, Show)

instance Semigroup (First' a) where
  a <> b =
    case (getFirst' a, getFirst' b) of
      (Nada, Nada) -> First' Nada
      (Only x, _)  -> First' (Only x)
      (_, Only x)  -> First' (Only x)

instance Monoid (First' a) where
  mempty = First' Nada

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary =
    frequency [(1, return $ First' Nada), (3, liftM (First' . Only) arbitrary)]

-- Trivial
data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

-- Identity
data Identity a =
  Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = liftM Identity arbitrary

-- Two
data Two a b =
  Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two a b) = Two (x <> a) (y <> b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

-- Properties
semigroupAssoc :: (Eq s, Semigroup s) => s -> s -> s -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstStr = First' String

type IdentStr = Identity String

type TwoStr = Two String String

-- Tests
spec :: Spec
spec = do
  describe "Semigroups" $ do
    describe "Trivial" $ do
      it "semigroup associativity should work" $
        property (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
    describe "Identity" $ do
      it "semigroup associativity should work" $
        property (semigroupAssoc :: IdentStr -> IdentStr -> IdentStr -> Bool)
    describe "Two" $ do
      it "semigroup associativity should work" $
        property (semigroupAssoc :: TwoStr -> TwoStr -> TwoStr -> Bool)
  describe "Monoids" $ do
    describe "Bull" $ do
      it "monoid associativity should work" $
        property (semigroupAssoc :: Bull -> Bull -> Bull -> Bool)
      it "monoid left identity should fail" $
        expectFailure $ property (monoidLeftIdentity :: Bull -> Bool)
      it "monoid right identity should fail" $
        expectFailure $ property (monoidRightIdentity :: Bull -> Bool)
    describe "First'" $ do
      it "monoid associativity should work" $
        property (semigroupAssoc :: FirstStr -> FirstStr -> FirstStr -> Bool)
      it "monoid left identity should work" $
        property (monoidLeftIdentity :: FirstStr -> Bool)
      it "monoid right identity should work" $
        property (monoidRightIdentity :: FirstStr -> Bool)
