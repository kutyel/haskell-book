module Test.Chapter15 where

import Chapter15
import Data.Monoid
import Test.Hspec
import Test.QuickCheck hiding (Failure, Success)

instance Arbitrary Bull where
  arbitrary = elements [Fools, Twoo]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = frequency [(1, pure $ First' Nada), (3, First' <$> arbitrary)]

instance Arbitrary Trivial where
  arbitrary = pure Trivial

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance
  (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c)
  where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance
  (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d)
  where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = Comp <$> arbitrary

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = oneof [Fst <$> arbitrary, Snd <$> arbitrary]

-- Arbitrary Optional
instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, pure Nada), (3, Only <$> arbitrary)]

-- Arbitrary Validation
instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = oneof [Failure <$> arbitrary, Success <$> arbitrary]

-- Properties
semigroupAssoc :: (Eq s, Semigroup s) => s -> s -> s -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

fAssoc :: (Eq a, Semigroup b) => (b -> t -> a) -> t -> b -> b -> b -> Bool
fAssoc f v a b c = f (a <> (b <> c)) v == f ((a <> b) <> c) v

-- Aliases
type FirstStr = First' String

type IdentStr = Identity String

type TwoStr = Two String String

type ThreeStr = Three String String String

type FourStr = Four String String String String

type StrOrStr = Or String String

type OprtStr = Optional String

type ValidStr = Validation String String

type CombStr = Combine String String

type CompStr = Comp String

-- Tests
spec :: Spec
spec = do
  describe "Chapter 15 exercises"
    $ it "madlibbin should work"
    $ madlibbin "damn" "quickly" "Jim" "beautiful"
      `shouldBe` "damn! he said quickly as he jumped into his car Jim and drove off with his beautiful wife."
  describe "Semigroups" $ do
    describe "Trivial"
      $ it "semigroup associativity should work"
      $ property (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
    describe "Identity"
      $ it "semigroup associativity should work"
      $ property (semigroupAssoc :: IdentStr -> IdentStr -> IdentStr -> Bool)
    describe "Two"
      $ it "semigroup associativity should work"
      $ property (semigroupAssoc :: TwoStr -> TwoStr -> TwoStr -> Bool)
    describe "Three"
      $ it "semigroup associativity should work"
      $ property (semigroupAssoc :: ThreeStr -> ThreeStr -> ThreeStr -> Bool)
    describe "Four"
      $ it "semigroup associativity should work"
      $ property (semigroupAssoc :: FourStr -> FourStr -> FourStr -> Bool)
    describe "BoolConj"
      $ it "semigroup associativity should work"
      $ property (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
    describe "BoolDisj"
      $ it "semigroup associativity should work"
      $ property (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
    describe "Combine"
      $ it "semigroup associativity should work"
      $ property
        (fAssoc unCombine :: String -> CombStr -> CombStr -> CombStr -> Bool)
    describe "Comp"
      $ it "semigroup associativity should work"
      $ property
        (fAssoc unComp :: String -> CompStr -> CompStr -> CompStr -> Bool)
    describe "Or"
      $ it "semigroup associativity should work"
      $ property (semigroupAssoc :: StrOrStr -> StrOrStr -> StrOrStr -> Bool)
    describe "Optional"
      $ it "semigroup associativity should work"
      $ property (semigroupAssoc :: OprtStr -> OprtStr -> OprtStr -> Bool)
    describe "Validation"
      $ it "semigroup associativity should work"
      $ property (semigroupAssoc :: ValidStr -> ValidStr -> ValidStr -> Bool)
  describe "Monoids" $ do
    describe "Bull" $ do
      it "monoid associativity should work" $
        property (semigroupAssoc :: Bull -> Bull -> Bull -> Bool)
      it "monoid left identity should fail"
        $ expectFailure
        $ property (monoidLeftIdentity :: Bull -> Bool)
      it "monoid right identity should fail"
        $ expectFailure
        $ property (monoidRightIdentity :: Bull -> Bool)
    describe "First'" $ do
      it "monoid associativity should work" $
        property (semigroupAssoc :: FirstStr -> FirstStr -> FirstStr -> Bool)
      it "monoid left identity should work" $
        property (monoidLeftIdentity :: FirstStr -> Bool)
      it "monoid right identity should work" $
        property (monoidRightIdentity :: FirstStr -> Bool)
    describe "Trivial" $ do
      it "monoid left identity should work" $
        property (monoidLeftIdentity :: Trivial -> Bool)
      it "monoid right identity should work" $
        property (monoidRightIdentity :: Trivial -> Bool)
    describe "Identity" $ do
      it "monoid left identity should work" $
        property (monoidLeftIdentity :: IdentStr -> Bool)
      it "monoid right identity should work" $
        property (monoidRightIdentity :: IdentStr -> Bool)
    describe "Two" $ do
      it "monoid left identity should work" $
        property (monoidLeftIdentity :: TwoStr -> Bool)
      it "monoid right identity should work" $
        property (monoidRightIdentity :: TwoStr -> Bool)
    describe "BoolConj" $ do
      it "monoid left identity should work" $
        property (monoidLeftIdentity :: BoolConj -> Bool)
      it "monoid right identity should work" $
        property (monoidRightIdentity :: BoolConj -> Bool)
    describe "BoolDisj" $ do
      it "monoid left identity should work" $
        property (monoidLeftIdentity :: BoolDisj -> Bool)
      it "monoid right identity should work" $
        property (monoidRightIdentity :: BoolDisj -> Bool)
    describe "Optional" $ do
      it "monoid left identity should work" $
        property (monoidLeftIdentity :: OprtStr -> Bool)
      it "monoid right identity should work" $
        property (monoidRightIdentity :: OprtStr -> Bool)
