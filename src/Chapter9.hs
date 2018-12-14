module Chapter9 where

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- EnumFromTo

eft :: (Eq a, Enum a) => a -> a -> [a]
eft x y = if x == y then [x] else x : eft (succ x) y

eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft

-- Comprehend Thy Lists

mySqr :: [Integer]
mySqr = [x^2 | x <- [1..10]] -- [1,4,9,16,25,36,49,64,81,100]

q1 :: [Integer]
q1 = [x | x <- mySqr, rem x 2 == 0] -- [4,16,36,64,100] ✅

q2 :: [(Integer, Integer)]
q2 = [(x, y) | x <- mySqr,
               y <- mySqr,
               x < 50, y > 50] -- [(1,64), (4,81), (9,100)] ❌ much longer!!!

q3 :: [(Integer, Integer)]
q3 = take 5 [(x, y) | x <- mySqr,
                      y <- mySqr,
                      x < 50, y > 50] -- [(1,64), (1,81), (1,100), (4,64), (4,81)] ❌
