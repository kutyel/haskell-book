module Chapter24 where

import Text.Parser.Combinators
import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

-- Parsing Practice

one = char '1'

one' = one >> stop

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

testParse' :: String -> Parser String -> IO ()
testParse' s p = print $ parseString p mempty s

testEOF :: Parser () -> IO ()
testEOF p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

-- 2
p123 :: String -> IO ()
p123 s = testParse' s $ choice [string "123", string "12", string "1"]

-- 3
-- TODO: write a Parser that does what "string" does, but using "char"
string2 :: Char -> Parser Char
string2 = undefined

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  -- 1
  pNL "oneEOF:"
  testEOF (one >> eof)
  pNL "oneTwoEOF:"
  testEOF (oneTwo >> eof)
  -- 2
  pNL "p123:"
  p123 "1"
  p123 "12"
  p123 "123"
