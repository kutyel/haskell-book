module Chapter11 where

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
getManu _ = Nothing

-- 4) bottom! (fixed with Maybe)

-- 5) add `size` to the Plane constructor!

-- Cardinality

-- 1) 1
data PugType = PugData

-- 2) cardinality of Airline -> 3

-- 3) Int8 -> 256, Int16 -> 65.536

-- 4) Int must be some type alias for a huge Int number and
-- Integer is a typeclass, so no lower or upper boundaries!!

-- 5) 256 is 2^8 so Int8 is an 8 bit-signed integer!
