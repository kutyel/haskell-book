module Chapter9 where

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- EnumFromTo

eft :: (Eq a, Enum a) => a -> a -> [a]
eft x y = if x == y then [x] else x : eft (succ x) y

eftBool :: Bool -> Bool -> [Bool]
eftBool x y = eft x y

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y = eft x y

eftInt :: Int -> Int -> [Int]
eftInt x y = eft x y

eftChar :: Char -> Char -> [Char]
eftChar x y = eft x y

-- TODO: Comprehend Thy Lists
