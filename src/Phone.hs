module Phone where

import Data.Char (isUpper, toUpper)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

type Digit = Char
type Presses = Int
type Values = String

data DaPhone = DaPhone [(Digit, Values)]

phone :: DaPhone
phone = DaPhone [
    ('1', "")
  , ('2', "ABC")
  , ('3', "DEF")
  , ('4', "GHI")
  , ('5', "JKL")
  , ('6', "MNO")
  , ('7', "PQRS")
  , ('8', "TUV")
  , ('9', "WXYZ")
  , ('*', "^")
  , ('0', " +_")
  , ('#', ".,")
  ]

convo :: [String]
convo =
  ["Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol ok. Have u ever tasted alcohol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "Ok. Do u think I am pretty Lol",
  "Lol ya",
  "Just making sure rofl ur turn"
  ]

indexOf :: Char -> String -> Int
indexOf x xs = (+1) $ fromJust $ elemIndex (toUpper x) xs

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone xs) x = (if isUpper x then [('*', 1)] else []) ++ foldr find [] xs
  where find (s, str) t = if elem (toUpper x) str then (s, indexOf x str) : t else t

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead p = concatMap (reverseTaps p)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr ((+) . snd) 0

mostPopularLetter :: String -> Char
mostPopularLetter = undefined

coolestLtr :: [String] -> Char
coolestLtr = undefined

coolestWord :: [String] -> String
coolestWord = undefined
