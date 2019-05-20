module Sing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString y = y ++ " over the rainbow"

sing =
  if (x > y)
    then fstString x
    else sndString y
  where
    x = "Singin"
    y = "Somewhere"
