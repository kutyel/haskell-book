module Test.Chapter15 where

import           Chapter15                      ( Optional(..) )
import           Control.Monad                  ( liftM )
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
  a <> b = case (getFirst' a, getFirst' b) of
    (Nada  , Nada  ) -> First' Nada
    (Only x, _     ) -> First' (Only x)
    (_     , Only x) -> First' (Only x)

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

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

-- Identity
data Identity a =
  Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = liftM Identity arbitrary

-- Two
data Two a b =
  Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two a b) = Two (x <> a) (y <> b)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

-- Three
data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>
         Semigroup (Three a b c) where
  (Three x y z) <> (Three a b c) = Three (x <> a) (y <> b) (z <> c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

-- Four
data Four a b c d =
  Four a b c d
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
         Semigroup (Four a b c d) where
  (Four w x y z) <> (Four a b c d) = Four (w <> a) (x <> b) (y <> c) (z <> d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four w x y z

-- BoolConj (and)
newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj x <> BoolConj y = case (x, y) of
    (True, True) -> BoolConj True
    (_   , _   ) -> BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = liftM BoolConj arbitrary

-- BoolDisj (or)
newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj x) <> (BoolDisj y) = case (x, y) of
    (_   , True) -> BoolDisj True
    (True, _   ) -> BoolDisj True
    (_   , _   ) -> BoolDisj False

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = liftM BoolDisj arbitrary

-- Or
data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  x <> y = case (x, y) of
    (Fst _, Snd x) -> Snd x
    (Fst _, Fst x) -> Fst x
    (Snd x, Fst _) -> Snd x
    (Snd x, Snd _) -> Snd x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [Fst x, Snd y]

-- Properties
semigroupAssoc :: (Eq s, Semigroup s) => s -> s -> s -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

-- Aliases
type FirstStr = First' String

type IdentStr = Identity String

type TwoStr = Two String String

type ThreeStr = Three String String String

type FourStr = Four String String String String

type StrOrStr = Or String String

-- Tests
spec :: Spec
spec = do
  describe "Semigroups" $ do
    describe "Trivial" $ do
      it "semigroup associativity should work"
        $ property (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
    describe "Identity" $ do
      it "semigroup associativity should work" $ property
        (semigroupAssoc :: IdentStr -> IdentStr -> IdentStr -> Bool)
    describe "Two" $ do
      it "semigroup associativity should work"
        $ property (semigroupAssoc :: TwoStr -> TwoStr -> TwoStr -> Bool)
    describe "Three" $ do
      it "semigroup associativity should work" $ property
        (semigroupAssoc :: ThreeStr -> ThreeStr -> ThreeStr -> Bool)
    describe "Four" $ do
      it "semigroup associativity should work"
        $ property (semigroupAssoc :: FourStr -> FourStr -> FourStr -> Bool)
    describe "BoolConj" $ do
      it "semigroup associativity should work" $ property
        (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
    describe "BoolDisj" $ do
      it "semigroup associativity should work" $ property
        (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
    describe "Or" $ do
      it "semigroup associativity should work" $ property
        (semigroupAssoc :: StrOrStr -> StrOrStr -> StrOrStr -> Bool)
  describe "Monoids" $ do
    describe "Bull" $ do
      it "monoid associativity should work"
        $ property (semigroupAssoc :: Bull -> Bull -> Bull -> Bool)
      it "monoid left identity should fail" $ expectFailure $ property
        (monoidLeftIdentity :: Bull -> Bool)
      it "monoid right identity should fail" $ expectFailure $ property
        (monoidRightIdentity :: Bull -> Bool)
    describe "First'" $ do
      it "monoid associativity should work" $ property
        (semigroupAssoc :: FirstStr -> FirstStr -> FirstStr -> Bool)
      it "monoid left identity should work"
        $ property (monoidLeftIdentity :: FirstStr -> Bool)
      it "monoid right identity should work"
        $ property (monoidRightIdentity :: FirstStr -> Bool)
    describe "Trivial" $ do
      it "monoid left identity should work"
        $ property (monoidLeftIdentity :: Trivial -> Bool)
      it "monoid right identity should work"
        $ property (monoidRightIdentity :: Trivial -> Bool)
    describe "Identity" $ do
      it "monoid left identity should work"
        $ property (monoidLeftIdentity :: IdentStr -> Bool)
      it "monoid right identity should work"
        $ property (monoidRightIdentity :: IdentStr -> Bool)
    describe "Two" $ do
      it "monoid left identity should work"
        $ property (monoidLeftIdentity :: TwoStr -> Bool)
      it "monoid right identity should work"
        $ property (monoidRightIdentity :: TwoStr -> Bool)
    describe "BoolConj" $ do
      it "monoid left identity should work"
        $ property (monoidLeftIdentity :: BoolConj -> Bool)
      it "monoid right identity should work"
        $ property (monoidRightIdentity :: BoolConj -> Bool)
    describe "BoolDisj" $ do
      it "monoid left identity should work"
        $ property (monoidLeftIdentity :: BoolDisj -> Bool)
      it "monoid right identity should work"
        $ property (monoidRightIdentity :: BoolDisj -> Bool)
