module Test.Chapter8Spec where

import Chapter8 (dividedBy, mult)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Testable (property))

spec :: Spec
spec =
  describe "Chapter8" $ do
    it "1 + 1 is greater than 1" $ (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ 2 + 2 `shouldBe` 4
    -- property testing!
    it "x + 1 is always greater than x" $ property $ \x -> x + 1 > (x :: Int)
    it "dividedBy should work" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
      dividedBy 22 5 `shouldBe` (4, 2)
    it "mult by recursive summation should work" $ do
      mult 7 3 `shouldBe` 7 * 3
      mult 5 5 `shouldBe` 5 * 5
      mult 4 3 `shouldBe` 4 * 3
