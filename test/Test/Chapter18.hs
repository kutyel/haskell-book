module Test.Chapter18 where

import Chapter17 (pureList)
import Chapter18
import Control.Applicative ((<**>), liftA2, liftA3)
import Control.Monad (join, liftM2)
import Data.Traversable (for)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes hiding (bind)

type Types = (Int, Bool, Double) -- this will be used to generate random values!

-- custom properties
prop_bind :: (Monad m, Eq (m b)) => (a -> m b) -> m a -> Bool
prop_bind f = liftA2 (==) (bind f) (f =<<)

prop_j :: (Monad m, Eq (m a)) => m (m a) -> Bool
prop_j = liftA2 (==) j join

prop_l1 :: (Monad m, Eq (m b)) => (a -> b) -> m a -> Bool
prop_l1 f = liftA2 (==) (l1 f) (f <$>)

prop_l2 :: (Monad m, Eq (m c)) => (a -> b -> c) -> m a -> m b -> Bool
prop_l2 f x y = l2 f x y == liftM2 f x y

prop_a :: (Monad m, Eq (m b)) => m a -> m (a -> b) -> Bool
prop_a xs fs = a xs fs == (xs <**> fs)

prop_meh :: (Monad m, Eq (m [b])) => [a] -> (a -> m b) -> Bool
prop_meh xs f = meh xs f == for xs f

prop_seq :: (Monad m, Eq (m [a])) => [m a] -> Bool
prop_seq = liftA2 (==) flipType sequence

-- arbitrary and eqprop instances
instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = oneof [First <$> arbitrary, Second <$> arbitrary]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

instance Arbitrary (Nope a) where
  arbitrary = pure NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = oneof [PLeft <$> arbitrary, PRight <$> arbitrary]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, pure Nil), (3, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

spec :: Spec
spec =
  describe "Chapter 18:" $ do
    it "twice when even should work with List monad" $
      twiceWhenEven [1 .. 3] `shouldBe` [1, 4, 4, 9]
    it "custom bind should work like flipped >>=" $
      property (prop_bind (\x -> [x, 1]) :: [Int] -> Bool)
    it "custom join should work exactly the same" $
      property (prop_j :: [[Int]] -> Bool)
    it "monad specific fmap should work exactly the same" $
      property (prop_l1 (+ 1) :: [Int] -> Bool)
    it "monad specific liftA2 should work exactly the same" $
      property (prop_l2 (==) :: [Int] -> [Int] -> Bool)
    it "monad specific apply should work exactly the same" $
      property (flip prop_a [(+ 1)] :: [Int] -> Bool)
    it "custom flip traverse (for) should work the same" $
      property (flip prop_meh pureList :: [Int] -> Bool)
    it "custom traverse should work exactly the same" $
      property (prop_seq :: [Maybe Int] -> Bool)
    it "Sum -> monad laws should hold!" $ do
      quickBatch $ applicative (undefined :: Sum Int Types)
      quickBatch $ monad (undefined :: Sum Int Types)
    it "Nope -> monad laws should hold!" $ do
      quickBatch $ applicative (undefined :: Nope Types)
      quickBatch $ monad (undefined :: Nope Types)
    it "BahEither -> monad laws should hold!" $ do
      quickBatch $ applicative (undefined :: BahEither Int Types)
      quickBatch $ monad (undefined :: BahEither Int Types)
    it "Identity -> monad laws should hold!" $ do
      quickBatch $ applicative (undefined :: Identity Types)
      quickBatch $ monad (undefined :: Identity Types)
    it "List -> monad laws should hold!" $ do
      quickBatch $ applicative (undefined :: List Types)
      quickBatch $ monad (undefined :: List Types)
