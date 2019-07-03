module Chapter16 where

-- 1) What is the kind of a? *
-- f :: a -> a
-- 2) What are the kinds of b and T? * -> *
-- f :: a -> b a -> T (b a)
-- 3) What is the kind of c? * -> * -> *
-- f :: c a b -> c b a
-- 1)
a = fmap (+ 1) $ read "[1]" :: [Int]

-- 2)
b = fmap (fmap (++ "lol")) (Just ["Hi,", "Hello"])

-- 3)
c = (* 2) . (\x -> x - 2)

-- 4)
d = ((return '1' ++) . show) . (\x -> [x,1 .. 3])

-- 5)
e :: IO Integer
e =
  let ioi = readIO "1" :: IO Integer
      changed = (fmap read . fmap ("123" ++) . fmap show) ioi
   in fmap (* 3) changed
