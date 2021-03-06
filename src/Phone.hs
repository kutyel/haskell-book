module Phone where

import Data.Char (isUpper, toUpper)
import Data.List (elemIndex, group, maximumBy, sort)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

type Digit = Char

type Presses = Int

type Values = String

newtype DaPhone = DaPhone [(Digit, Values)]

phone :: DaPhone
phone =
  DaPhone
    [ ('1', ""),
      ('2', "ABC"),
      ('3', "DEF"),
      ('4', "GHI"),
      ('5', "JKL"),
      ('6', "MNO"),
      ('7', "PQRS"),
      ('8', "TUV"),
      ('9', "WXYZ"),
      ('*', "^"),
      ('0', " +_"),
      ('#', ".,")
    ]

convo :: [String]
convo =
  [ "Wanna play 20 questions",
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
indexOf x xs = (+ 1) $ fromJust $ elemIndex (toUpper x) xs

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone xs) x =
  [('*', 1) | isUpper x] ++ foldr find [] xs
  where
    find (s, str) t = if toUpper x `elem` str then (s, indexOf x str) : t else t

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead p = concatMap (reverseTaps p)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr ((+) . snd) 0

mostPopularLetter :: String -> Char
mostPopularLetter = maximumBy $ comparing $ fingerTaps . reverseTaps phone

coolest :: Ord a => [a] -> a
coolest = head . maximumBy (comparing length) . group . sort

coolestLtr :: [String] -> Char
coolestLtr = coolest . concat

coolestWord :: [String] -> String
coolestWord = coolest . concatMap words
