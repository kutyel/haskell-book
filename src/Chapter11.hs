{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chapter11 where

import           Data.Char       (toUpper)
import           Data.Int
import           Data.List       (intersperse)
import           Data.List.Split (splitOn)

data DogueDeBordeaux dogue =
  DogueDeBordeaux dogue

data Doggies a
  = Husky a
  | Mastiff a
  deriving (Eq, Show)

-- Dog Types
-- 1) Doggies -> type constructor
-- 2) * -> *
-- 3) *
-- 4) Num a => Doggies a
-- 5) Doggies Integer
-- 6) Doggies String
-- 7) DogueDeBordeaux is both a type constructor and a data constructor
-- 8) dogue -> DogueDeBordeaux dogue
-- 9) DogueDeBordeaux String
data Price =
  Price Integer
  deriving (Eq, Show)

-- Vehicles
data Manufacturer
  = Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline
  = PapuAir
  | CatatpultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle
  = Car Manufacturer Price
  | Plane Airline String
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)

urCar = Car Mazda (Price 20000)

clownCar = Car Tata (Price 7000)

doge = Plane PapuAir "Small"

-- 1) myCar :: Vehicle
-- 2)
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _           = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3)
getManu :: Vehicle -> Maybe Manufacturer
getManu (Car a _) = Just a
getManu _         = Nothing

-- 4) bottom! (fixed with Maybe)
-- 5) add `size` to the Plane constructor!
-- Cardinality
-- 1) 1
data PugType =
  PugData

-- 2) cardinality of Airline -> 3
-- 3) Int8 -> 256, Int16 -> 65.536
-- 4) Int is a type alias for Int32 or Int64 whereas
-- Integer is limited only by your machine's memory!
-- 5) 256 is 2^8 so Int8 is an 8 bit-signed integer!
-- For Example
data Example =
  MakeExample
  deriving (Show)

-- 1) MakeExample :: Example. :t Example -> constructor not in scope!
-- 2) It works, has an instance of the Show typeclass.
data Example2 =
  MakeExample2 Int
  deriving (Show)

-- 3) :t MakeExample2 :: Int -> Example 2. It changed the kind!
-- Logic Goats
class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats =
  Goats Int
  deriving (Eq, Show, TooMany)

-- 1)
instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

-- 2)
instance TooMany (Int, Int) where
  tooMany (x, y) = x + y > 42

-- 3)
instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany x || tooMany y

-- Pity the Bool
data BigSmall
  = Big Bool
  | Small Bool
  deriving (Eq, Show)

-- 1) cardinality -> 2 + 2 = 4
data NumberOrBool
  = Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)

-- 2) cardinality -> 256 + 2 = 258
-- Literal 128 is out of the Int8 range -128..127
-- Literal -129 is out of the Int8 range -128..127
myNumba = Numba (-128) -- no warning here!

-- How Does Your Garden Grow?
type Gardener = String

data Garden
  = Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
  deriving (Show)

-- Programmers
data OperatingSystem
  = GnuPlusLinux
  | OpenBSD
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer
    { os   :: OperatingSystem
    , lang :: ProgLang
    }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux, OpenBSD, Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers =
  [Programmer os lang | os <- allOperatingSystems, lang <- allLanguages]

-- The Quad
data Quantum
  = Yes
  | No
  | Both
  deriving (Eq, Show)

convert1 :: Quantum -> Bool
convert1 Yes  = True
convert1 No   = True
convert1 Both = True

convert2 :: Quantum -> Bool
convert2 Yes  = True
convert2 No   = True
convert2 Both = False

convert3 :: Quantum -> Bool
convert3 Yes  = True
convert3 No   = False
convert3 Both = False

convert4 :: Quantum -> Bool
convert4 Yes  = True
convert4 No   = False
convert4 Both = True

convert5 :: Quantum -> Bool
convert5 Yes  = False
convert5 No   = True
convert5 Both = False

convert6 :: Quantum -> Bool
convert6 Yes  = False
convert6 No   = False
convert6 Both = True

convert7 :: Quantum -> Bool
convert7 Yes  = False
convert7 No   = True
convert7 Both = True

convert8 :: Quantum -> Bool
convert8 Yes  = False
convert8 No   = False
convert8 Both = False

data Quad
  = One
  | Two
  | Three
  | Four
  deriving (Eq, Show)

-- 1)
eQuad :: Either Quad Quad
eQuad = undefined -- (4 + 4) = 8

-- 2)
prodQuad :: (Quad, Quad)
prodQuad = undefined -- (4 * 4) = 16

-- 3)
funcQuad :: Quad -> Quad
funcQuad = undefined -- (4 ^ 4) = 256

-- 4)
prodTBool :: (Bool, Bool, Bool)
prodTBool = undefined -- (2 * 2 * 2) = 8

-- 5)
gTwo :: Bool -> Bool -> Bool
gTwo = undefined -- ((2 ^ 2) ^ 2) = 16

-- 6)
fTwo :: Bool -> Quad -> Quad
fTwo = undefined -- ((2 ^ 4) ^ 4) = 65.536

-- Binary Tree
data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert b Leaf = Node Leaf b Leaf
insert b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert b left) a right
  | b > a = Node left a (insert b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf                = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay =
  if mapTree (+ 1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

-- Chapter exercises
data Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday

-- 1) we can say...
-- a) `Weekday` is a type with five data constructors
-- 2) what is the type of f? c)
f :: Weekday -> String
f Friday = "Miller Time"

-- 3) types defined with the `data` keyword...
-- b) must begin with a capital letter
-- 4) the function g xs = xs !! (length xs - 1)
g :: [a] -> a
g xs = xs !! (length xs - 1)

-- c) delivers the final element of xs
-- As-patterns
-- 1
isSubseqOf :: Eq a => [a] -> [a] -> Bool
isSubseqOf [] _            = True
isSubseqOf (x:xs) y@(_:ys) = elem x y && isSubseqOf xs ys

-- 2
capitalizeWords :: String -> [(String, String)]
capitalizeWords = map toTuple . words
  where
    toTuple s@(x:xs) = (s, toUpper x : xs)

-- Language exercises
-- 1
capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs

-- 2
capitalizeParagraph :: String -> String
capitalizeParagraph =
  concat . (intersperse ". ") . map capitalizeWord . (splitOn ". ")
