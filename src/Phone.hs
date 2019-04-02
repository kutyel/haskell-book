module Phone where

type Digit = Char
type Presses = Int
type Values = String

data Taps = Taps [(Digit, Presses)]
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
  , ('0', "*_")
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

reverseTaps :: DaPhone -> Char -> Taps
reverseTaps = undefined

cellPhonesDead :: DaPhone -> String -> Taps
cellPhonesDead = undefined

fingerTaps :: Taps -> Presses
fingerTaps = undefined

mostPopularLetter :: String -> Char
mostPopularLetter = undefined

coolestLtr :: [String] -> Char
coolestLtr = undefined

coolestWord :: [String] -> String
coolestWord = undefined
