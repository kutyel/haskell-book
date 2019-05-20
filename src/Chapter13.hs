module Chapter13 where

import           Control.Applicative     (liftA2)
import qualified Control.Concurrent      as CC
import qualified Control.Concurrent.MVar as MV
import           Control.Exception       (mask, try)
import           Control.Monad           (forever, when)
import           Data.Bits
import           Data.Char               (toLower)
import           Data.List.Split         (chunksOf)
import qualified Data.Time.Clock.POSIX   as PSX
import           System.Exit             (exitSuccess)
import           System.IO               (BufferMode (NoBuffering),
                                          hSetBuffering, stdout)
import           System.IO.Unsafe        (unsafePerformIO)

-- 1) forever, when
-- 2) Data.Bits, Database.Blacktip.Types
-- 3) all the types from the ADTs and Database
-- 4)
-- writeTimestamp :: MV.MVar ServerState -> FPC.FilePath -> IO CC.ThreadId
-- writeTimestamp s path = do
--   CC.forkIO go
--   where
--     go =
--       forever $ do
--         ss <- MV.readMVar s
--         mask $ \_ -> do
--           FS.writeFile path
--           (B.pack (show (ssTime ss)))
--         -- sleep for 1 second
--         CC.threadDelay 1000000
-- a) Control.Concurrent.MVar, Filesystem.Path.CurrentOS and Control.Concurrent
-- b) Filesystem
-- c) Control.Monad
-- Modifying code
isPalindrome :: String -> Bool
isPalindrome =
  (liftA2 (==) reverse id) . filter (flip notElem "' ") . map toLower

palindrome :: IO ()
palindrome =
  forever $ do
    line <- getLine
    if isPalindrome line
      then putStrLn "It's a palindrome!"
      else exitSuccess

-- 4)
type Name = String

type Age = Integer

data Person =
  Person Name Age
  deriving (Show)

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
    Left $
    PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStrLn $ "Enter a name: "
  name <- getLine
  putStrLn $ "Enter an age: "
  age <- getLine
  case mkPerson name (read age) of
    Left error -> putStrLn $ "Error occured: " ++ show error
    Right person -> putStrLn $ "Yay! Successfully got a person: " ++ show person
