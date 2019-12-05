module Cipher where

import Data.Char (chr, isUpper, ord)
import System.IO

offset :: Char -> Int
offset x = ord $ if isUpper x then 'A' else 'a'

shift :: Char -> Int
shift x = ord x - offset x

cipher :: Int -> Char -> Char
cipher n c = chr $ mod (shift c + n) 26 + offset c

toCaesar :: String -> String
toCaesar = map $ cipher 3

fromCaesar :: String -> String
fromCaesar = map $ cipher (-3)

toVigenere :: String -> String -> String
toVigenere secret = zipWith (cipher . shift) (cycle secret) . concat . words

fromVigenere :: String -> String -> String
fromVigenere secret =
  zipWith (cipher . negate . shift) (cycle secret) . concat . words

-- IO
toCaesarIO :: IO ()
toCaesarIO = do
  line <- getLine
  putStrLn $ toCaesar line

fromCaesarIO :: IO ()
fromCaesarIO = do
  line <- getLine
  putStrLn $ fromCaesar line

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
