module Chapter6 where

import Data.List (sort)

-- 1

data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2

data Mood = Blah | Woot deriving (Eq, Show)

settleDown x = if x == Woot then Blah else x

-- 3
-- a) settleDown :: Mood -> Mood (only `Blah` or `Woot`)
-- b) compiler error cause there's no instance of `Num` in `Mood`
-- c) compiler error because `Mood` does not have an `Ord` instance

-- 4

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

-- type checks!! s1 :: Object -> Sentence

-- Given a datatype declaration, what can we do?

data Rocks = Rocks String deriving (Eq, Show, Ord)

data Yeah = Yeah Bool deriving (Eq, Show, Ord)

data Papu = Papu Rocks Yeah deriving (Eq, Show, Ord)

-- 1

phew = Papu (Rocks "Chases") (Yeah True)

-- 2

truth = Papu (Rocks "chomskydox") (Yeah True)

-- 3

equalityForAll :: Papu -> Papu -> Bool
equalityForAll x x' = x == x'

-- 4

comparePapus :: Papu -> Papu -> Bool
comparePapus y y' = y > y'

-- Match the types

-- 1
-- a)
i :: Num a => a
i = 1
-- b)
-- i :: a <- does not work

-- 2
-- a)
f :: Float
f = 1.0
-- b) f :: Num a => a <- does not work (Fractional)

-- 3
-- a)
f1 :: Fractional a => a
f1 = 1.0
-- b) does work 👌🏼

-- 4
-- a)
f2 :: RealFrac a => a
f2 = 1.0
-- b) does work 👌🏼 (inherits from Fractional)

-- 5
-- a)
freud :: Ord a => a -> a
freud x = x
-- b) does work 👌🏼 (although is more specific)

-- 6
-- a)
freud' :: Int -> Int
freud' x = x
-- b) does work 👌🏼 (although is more specific)

-- 7
-- a)
myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX
-- b) does not work! (because the type is already specific)

-- 8
-- a)
myY = 1 :: Int

sigmund' :: Int -> Int
sigmund' y = myY
-- b) does not work! (because the type is already specific)

-- 9
-- a)
jung :: [Int] -> Int
jung xs = head (sort xs)
-- b) does work 👌🏼 (although is more specific)

-- 10
-- a)
young :: Ord a => [a] -> a
young xs = head (sort xs)
-- b) does work 👌🏼 (although is more general)

-- 11
-- a)
mySort :: [Char] -> [Char]
mySort = sort

signifier :: String -> Char
signifier xs = head (mySort xs)
-- b) [signifier :: Ord a => [a] -> a] <- does not work!
-- (`mySort` is more specific than `sort`!)
