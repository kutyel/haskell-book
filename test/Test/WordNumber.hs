module Test.WordNumber where

import Test.Hspec
import WordNumber (digitToWord, digits, main, wordNumber)

spec :: Spec
spec =
  describe "WordNumber" $ do
    describe "digitToWord" $ do
      it "returs zero for 0" $ digitToWord 0 `shouldBe` "zero"
      it "returns one for 1" $ digitToWord 1 `shouldBe` "one"
      it "returns number for n > 9" $ digitToWord 10 `shouldBe` "number"
    describe "digits" $ do
      it "returns [1] for 1" $ digits 1 `shouldBe` [1]
      it "returns [1, 0, 0] for 100" $ digits 100 `shouldBe` [1, 0, 0]
    describe "wordNumber" $ do
      it "one-seven-eight for 78" $ wordNumber 178 `shouldBe` "one-seven-eight"
      it "one-zero-zero given 100" $ wordNumber 100 `shouldBe` "one-zero-zero"
      it "nine-zero-zero-one for 9001" $
        wordNumber 9001 `shouldBe` "nine-zero-zero-one"
      it "one-two-three-two-four-five-four-six for 12324546" $
        wordNumber 12324546 `shouldBe` "one-two-three-two-four-five-four-six"
      it "main should work" $
        main `shouldReturn` "one-two-three-two-four-five-four-six"
