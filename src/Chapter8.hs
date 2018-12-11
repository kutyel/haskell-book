module Chapter8 where

-- Intermission

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b =
  (f . applyTimes (n - 1) f) b

-- applyTimes 5 (+1) 5 <- start!
-- applyTimes 4 (+1) 6 <- 1st iteration
-- applyTimes 3 (+1) 7 <- 2nd iteration
-- applyTimes 2 (+1) 8 <- 3rd iteration
-- applyTimes 1 (+1) 9 <- 4th iteration
-- applyTimes 0 (+1) 10 -> returns 10 (pattern match)

-- Chapter Exercises

-- Review of types
-- 1) typeof [[True, False], [True, True], [False, True]]
-- d) [[Bool]]
-- 2) which has the same type as the above?
-- b) [[3 == 3], [6 > 5], [3 < 4]]
-- 3) which of the following is true?
func :: [a] -> [a] -> [a]
func x y = x ++ y
-- d) all of the above!
-- 4) which is a valid application of `func`?
-- a) func "Hello" "World"

-- Reviewing Curry

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- 1) appedCatty "woohoo!" -> "woops mrow woohoo!" ✅
-- 2) frappe "1" -> "1 mrow haha" ✅
-- 3) frappe (appedCatty "blue") -> "woops mrow blue mrow haha" ✅
-- 4) appedCatty (frappe "blue") -> "woops mrow blue mrow haha" ✅
-- 5) cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))
-- -> "pink mrow haha mrow green mrow woops mrow blue" ✅
-- 6) cattyConny (flippy "Pugs" "are") "awesome" -> "are mrow Pugs mrow awesome" ✅

-- Recursion

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise =
            go (n - d) d (count + 1)

-- dividedBy 15 2
-- go 15 2 0
-- go (15 - 2) 2 (0 + 1)
-- (n == 13, d == 2, count == 1)
-- go 13 2 1
-- go (13 - 2) 2 (1 + 1)
-- (n == 11, d == 2, count == 2)
-- go 11 2 2
-- go (11 - 2) 2 (2 + 1)
-- (n == 9, d == 2, count == 3)
-- go 9 2 3
-- go (9 - 2) 2 (3 + 1)
-- (n == 7, d == 2, count == 4)
-- go 7 2 4
-- go (7 - 2) 2 (4 + 1)
-- (n == 5, d == 2, count == 5)
-- go 5 2 5
-- go (5 - 2) 2 (5 + 1)
-- (n == 3, d == 2, count == 6)
-- go 3 2 6
-- go (3 - 2) 2 (6 + 1)
-- (n == 1, d == 2, count == 7)
-- | 1 < 2 = (7, 1) 🚀

-- 2) recursicely sum all numbers

recSum :: (Eq a, Num a) => a -> a
recSum 0 = 0
recSum n = n + recSum (n - 1)
-- recSum 5 -> 15 🎉

-- 3) mult with recursive summation

mult :: Integral a => a -> a -> a
mult x y = go x x y
  where go acc m count
          | count == 1 = acc
          | otherwise = go (acc + m) m (count - 1)

-- TODO: Fixing dividedBy

-- TODO: McCarthy 91 function
