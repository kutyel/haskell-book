module Test.Chapter17 where

import           Chapter17
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

tuple :: (String, String, String)
tuple = ("a", "a", "a")

-- tests
spec :: Spec
spec = do
  describe "Applicatives!" $ do
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
    describe "Identity" $ do
      it "applicative laws should hold" $
        quickBatch $ applicative $ Identity tuple
    -- describe "Constant" $ do
    --   it "applicative laws should hold" $
    --     quickBatch $ applicative $ Constant tuple
