module Test.CipherSpec where

import Cipher (fromCaesar, fromVigenere, toCaesar, toVigenere)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck
  ( Gen,
    Property,
    Testable (property),
    elements,
    forAll,
    listOf,
    listOf1,
  )

charGen :: Gen Char
charGen = elements ['a' .. 'z']

strGen :: Gen String
strGen = listOf charGen

strNonEmptyGen :: Gen String
strNonEmptyGen = listOf1 charGen

prop_caesarCipher :: Property
prop_caesarCipher = forAll strGen (\s -> (fromCaesar . toCaesar) s == s)

prop_vigenereCipher :: Property
prop_vigenereCipher =
  forAll strNonEmptyGen (\s -> (fromVigenere s . toVigenere s) s == s)

spec :: Spec
spec =
  describe "Cihpers" $ do
    it "Caesar cipher should work" $ property prop_caesarCipher
    it "Vigenere cipher should work" $ property prop_vigenereCipher
