module Chapter22 where

import           Data.Char

-- warming up!
cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = rev . cap

fmapped :: String -> String
fmapped = rev <$> cap

tupled :: String -> (String, String)
tupled = (,) <$> cap <*> rev

-- monads, monads everywhere!
tupled' :: String -> (String, String)
tupled' = do
  reversed <- rev
  capitalised <- cap
  return (reversed, capitalised)

tupled'' :: String -> (String, String)
tupled'' = rev >>= \r -> cap >>= \c -> pure (r, c)
