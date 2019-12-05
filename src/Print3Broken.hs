module Print3Broken where

greeting = "Yarrrrr"

printSecond :: IO ()
printSecond = putStrLn greeting

main :: IO ()
main = do
  putStrLn greeting
  printSecond

-- Exercises!
-- a)
exclamate :: String -> String
exclamate x = x ++ "!"

-- b)
fifthLetter :: String -> Char
fifthLetter x = x !! 4

-- c)
lastWord :: String -> String
lastWord = drop 9

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x
