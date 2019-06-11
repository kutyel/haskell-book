module Test.Chapter15 where

import           Data.Monoid
import           Test.Hspec
import           Test.QuickCheck

data Bull
  = Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Semigroup Bull where
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

spec :: Spec
spec = do
  let ma = monoidAssoc
      mli = monoidLeftIdentity
      mri = monoidRightIdentity
  describe "Monoids" $ do
    describe "Bull" $ do
      it "monoid associativity should work" $
        property (ma :: Bull -> Bull -> Bull -> Bool)
      it "monoid left identity should fail" $
        expectFailure $ property (mli :: Bull -> Bool)
      it "monoid right identity should fail" $
        expectFailure $ property (mri :: Bull -> Bool)
