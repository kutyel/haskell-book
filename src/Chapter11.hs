{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chapter11 where

import Data.Int

data DogueDeBordeaux dogue = DogueDeBordeaux dogue

data Doggies a =
    Husky a
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

data Price = Price Integer deriving (Eq, Show)

-- Vehicles

data Manufacturer =
    Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline =
    PapuAir
  | CatatpultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
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
data PugType = PugData

-- 2) cardinality of Airline -> 3

-- 3) Int8 -> 256, Int16 -> 65.536

-- 4) Int is a type alias for Int32 or Int64 whereas
-- Integer is limited only by your machine's memory!

-- 5) 256 is 2^8 so Int8 is an 8 bit-signed integer!

-- For Example

data Example = MakeExample deriving Show

-- 1) MakeExample :: Example. :t Example -> constructor not in scope!

-- 2) It works, has an instance of the Show typeclass.

data Example2 = MakeExample2 Int deriving Show

-- 3) :t MakeExample2 :: Int -> Example 2. It changed the kind!

-- Logic Goats

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)

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

data BigSmall =
    Big Bool
  | Small Bool
  deriving (Eq, Show)

-- 1) cardinality -> 2 + 2 = 4

data NumberOrBool =
  Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)

-- 2) cardinality -> 256 + 2 = 258
-- Literal 128 is out of the Int8 range -128..127
-- Literal -129 is out of the Int8 range -128..127
myNumba = Numba (-128) -- no warning here!

-- How Does Your Garden Grow?

type Gardener = String

data Garden = Gardenia Gardener
            | Daisy Gardener
            | Rose Gardener
            | Lilac Gardener
            deriving Show
