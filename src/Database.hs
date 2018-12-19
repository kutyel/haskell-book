module Database where

import Data.Bool
import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

-- 1)
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr (\x xs -> case x of
  DbDate x -> x : xs
  _        -> xs) []

-- 2)
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr (\x xs -> case x of
  DbNumber x -> x : xs
  _          -> xs) []

-- 3)
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = f . filterDbDate
  where f (x:xs) = foldr (\a b -> bool b a (a > b)) x xs

-- 4)
sumDb :: [DatabaseItem] -> Integer
sumDb = (foldr (+) 0) . filterDbNumber

-- 5)
avgDb :: [DatabaseItem] -> Double
avgDb xs = (fromIntegral (sumDb xs)) / (fromIntegral (length (filterDbNumber xs)))
