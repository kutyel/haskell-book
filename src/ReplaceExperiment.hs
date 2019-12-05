module ReplaceExperiment where

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe String]
lms = [Just "Ave", Nothing, Just "woohoo"]

-- more specific argument
replaceWithP' :: [Maybe String] -> Char
replaceWithP' = replaceWithP

-- lifted
liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

-- specific
liftedReplace' :: [Maybe String] -> String
liftedReplace' = liftedReplace

-- twice
twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

-- more specific
twiceLifted' :: [Maybe String] -> [Maybe Char]
twiceLifted' = twiceLifted

-- thrice
thriceLifted ::
  (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

-- more concrete
thriceLifted' :: [Maybe String] -> [Maybe String]
thriceLifted' = thriceLifted

main :: IO ()
main = do
  putStr "replaceWithP' lms:   "
  print (replaceWithP' lms)
  putStr "liftedReplace lms:   "
  print (liftedReplace lms)
  putStr "liftedReplace' lms:  "
  print (liftedReplace' lms)
  putStr "twiceLifted lms:     "
  print (twiceLifted lms)
  putStr "twiceLifted' lms:    "
  print (twiceLifted' lms)
  putStr "thriceLifted lms:    "
  print (thriceLifted lms)
  putStr "thriceLifted' lms:   "
  print (thriceLifted' lms)
