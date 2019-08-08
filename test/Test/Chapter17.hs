module Test.Chapter17 where

import           Chapter17
import           Test.Hspec
import           Test.QuickCheck          hiding (Failure, Success)
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

type Types = (Int, Bool, Double) -- this will be used to generate random values!

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Option a) where
  arbitrary = frequency [(1, pure None), (3, Some <$> arbitrary)]

instance Eq a => EqProp (Option a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, pure Nil), (3, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = oneof [Failure <$> arbitrary, Success <$> arbitrary]

instance (Eq a, Eq b) => EqProp (Validation a b) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

-- tests
spec :: Spec
spec =
  describe "Chapter 17:" $ do
    it "f 3 should be Just 'hello'" $ f 3 `shouldBe` Just "hello"
    it "g 8 should be Just 'chris'" $ g 8 `shouldBe` Just "chris"
    it "(+) <$> h 5 <*> m 1 should be Just 9007" $
      (+) <$> h 5 <*> m 1 `shouldBe` Just 9007
    it "(+) <$> h 5 <*> m 6 should be Nothing" $
      (+) <$> h 5 <*> m 6 `shouldBe` Nothing
    it "foo should be Just 'hellosup?'" $ foo `shouldBe` Just "hellosup?"
    it "length of foo should be Just 9" $ l `shouldBe` Just 9
    it "added should be Just 9" $ added `shouldBe` Just 9
    it "tupled should be Just (6, 5)" $ tupled `shouldBe` Just (6, 5)
    it "maxed should be Just 3" $ maxed `shouldBe` Just 3
    it "summed should be Just 5" $ summed `shouldBe` Just 5
    it "sum 2 + 3 should be Just 5" $ sum <$> sequence [x, y'] `shouldBe` Just 5
    it "sum everything and should be Just 20" $
      sum <$> sequence [added, y, z] `shouldBe` Just 20
    it "fixer upper const should Just Hello" $ ex1 `shouldBe` Just "Hello"
    it "fixer upper (,,,) should Just (,,,)" $
      ex2 `shouldBe` Just (90, 10, "Tierness", [1, 2, 3])
    it "refactor `combos` to use applicatives!" $
      ex `shouldBe` [(x, y, z) | x <- stops, y <- vowels, z <- stops]
    it "own version of ZipList' should work" $
      let z = ZipList' $ toMyList [(+ 9), (* 2), (+ 8)]
          z' = ZipList' $ toMyList [1 .. 3]
       in z <*> z' `shouldBe` (ZipList' $ toMyList [10, 4, 11])
    it "Identity -> applicative laws should hold!" $
      quickBatch $ applicative (undefined :: Identity Types)
    it "Constant -> applicative laws should hold!" $
      quickBatch $ applicative (undefined :: Constant String Types)
    it "Option -> applicative laws should hold!" $
      quickBatch $ applicative (undefined :: Option Types)
    it "List -> applicative laws should hold!" $
      quickBatch $ applicative (undefined :: List Types)
    it "ZipList' -> applicative laws should hold!" $
      quickBatch $ applicative (undefined :: ZipList' Types)
    it "Validation -> applicative laws should hold!" $
      quickBatch $ applicative (undefined :: Validation String Types)
    it "Pair -> applicative laws should hold!" $
      quickBatch $ applicative (undefined :: Pair Types)
    it "Two -> applicative laws should hold!" $
      quickBatch $ applicative (undefined :: Two String Types)
    it "Three -> applicative laws should hold!" $
      quickBatch $ applicative (undefined :: Three String String Types)
    it "Three' -> applicative laws should hold!" $
      quickBatch $ applicative (undefined :: Three' String Types)
    it "Four -> applicative laws should hold!" $
      quickBatch $ applicative (undefined :: Four String String String Types)
    it "Four' -> applicative laws should hold!" $
      quickBatch $ applicative (undefined :: Four' String Types)
