module PoemLines where

split :: Char -> String -> [String]
split _ [] = []
split c xs = takeWhile (/= c) xs : split c ((drop 1 . dropWhile (/= c)) xs)

myWords :: String -> [String]
myWords = split ' '

firstSen = "Tyger Tyger, burn it bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines = split '\n'

shouldEqual =
  [ "Tyger Tyger, burn it bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $
  "Are they equal? "
  ++ show (myLines sentences == shouldEqual)
