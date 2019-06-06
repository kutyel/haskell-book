module Test.WordNumber where

import           Test.Hspec
import           WordNumber (digitToWord, digits, wordNumber)

spec :: Spec
spec = do
  describe "WordNumber" $ do
    describe "digitToWord" $ do
      it "returs zero for 0" $ do digitToWord 0 `shouldBe` "zero"
      it "returns one for 1" $ do digitToWord 1 `shouldBe` "one"
    describe "digits" $ do
      it "returns [1] for 1" $ do digits 1 `shouldBe` [1]
      it "returns [1, 0, 0] for 100" $ do digits 100 `shouldBe` [1, 0, 0]
    describe "wordNumber" $ do
      it "one-zero-zero given 100" $ do
        wordNumber 100 `shouldBe` "one-zero-zero"
      it "nine-zero-zero-one for 9001" $ do
        wordNumber 9001 `shouldBe` "nine-zero-zero-one"
      it "one-two-three-two-four-five-four-six for 12324546" $ do
        wordNumber 12324546 `shouldBe` "one-two-three-two-four-five-four-six"