module Chapter29 where

import Data.Char (chr, isUpper, ord)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

offset :: Char -> Int
offset x = ord $ if isUpper x then 'A' else 'a'

shift :: Char -> Int
shift x = ord x - offset x

cipher :: Int -> Char -> Char
cipher n c = chr $ mod (shift c + n) 26 + offset c

toVigenere :: String -> String -> String
toVigenere secret = zipWith (cipher . shift) (cycle secret) . concat . words

fromVigenere :: String -> String -> String
fromVigenere secret =
  zipWith (cipher . negate . shift) (cycle secret) . concat . words

-- IO

getInput :: IO String
getInput = do
  hSetBuffering stdout NoBuffering
  putStrLn "Enter a secret:"
  getLine

toVigenereIO :: IO ()
toVigenereIO = do
  secret <- getInput
  putStrLn "Enter a message to encrypt:"
  message <- getLine
  putStrLn $ toVigenere secret message

fromVigenereIO :: IO ()
fromVigenereIO = do
  secret <- getInput
  putStrLn "Enter a message to decrypt:"
  message <- getLine
  putStrLn $ fromVigenere secret message

-- TODO: Chapter 29 Exercises -----------------------------
-- Make an executable that takes a key and a mode argument:
--  * If the mode is -d, the executable decrypts the input from standard in
--    and writes the decrypted text to standard out.
--  * If the mode is -e, the executable blocks on input from standard input (stdin)
--    and writes the encrypted output to standard output (stdout).
main :: IO ()
main = putStrLn "please complete this exercise!"
