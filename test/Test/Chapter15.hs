module Test.Chapter15 where

import           Chapter15       (Optional (..))
import           Control.Monad   (liftM)
import           Data.Monoid
import           Test.Hspec
import           Test.QuickCheck

data Bull
  = Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = elements [Fools, Twoo]

instance Semigroup Bull where
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools

newtype First' a =
  First'
    { getFirst' :: Optional a
    }
  deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) a b =
    case (getFirst' a, getFirst' b) of
      (Nada, Nada) -> First' Nada
      (Only x, _)  -> First' (Only x)
      (_, Only x)  -> First' (Only x)

instance Monoid (First' a) where
  mempty = First' Nada

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary =
    frequency [(1, return $ First' Nada), (3, liftM (First' . Only) arbitrary)]

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstStr = First' String

spec :: Spec
spec = do
  describe "Monoids" $ do
    describe "Bull" $ do
      it "monoid associativity should work" $
        property (monoidAssoc :: Bull -> Bull -> Bull -> Bool)
      it "monoid left identity should fail" $
        expectFailure $ property (monoidLeftIdentity :: Bull -> Bool)
      it "monoid right identity should fail" $
        expectFailure $ property (monoidRightIdentity :: Bull -> Bool)
    describe "First'" $ do
      it "monoid associativity should work" $
        property (monoidAssoc :: FirstStr -> FirstStr -> FirstStr -> Bool)
      it "monoid left identity should work" $
        property (monoidLeftIdentity :: FirstStr -> Bool)
      it "monoid right identity should work" $
        property (monoidRightIdentity :: FirstStr -> Bool)
