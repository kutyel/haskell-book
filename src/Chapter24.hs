module Chapter24 where

import Control.Applicative ((<|>))
import Data.Ratio ((%))
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
-- write a Parser that does what "string" does, but using "char"
string2 :: String -> Parser String
string2 = traverse char

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

-- units of success
myParser :: Parser Integer
myParser = integer >>= \num -> eof >> return num

ex2 = parseString myParser mempty "123"

ex3 = parseString myParser mempty "123abc" -- should fail

-- try try
parseFraction :: Parser Rational
parseFraction = (%) <$> decimal <*> (char '/' *> decimal)

intOrFraction :: Parser (Either Rational Integer)
intOrFraction = try (Left <$> parseFraction) <|> (Right <$> integer)

-- Chapter exercises
-- 1) Parse Semantic Versioning! http://semver.org
data NumberOrString
  = NOSS String
  | NOSI Integer
  deriving (Eq, Show)

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer
  = SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

parseNOS :: Parser NumberOrString
parseNOS =
  skipMany (char '.')
    >> (NOSI <$> try (decimal <* notFollowedBy letter))
    <|> (NOSS <$> some (letter <|> digit))

parseSemVer :: Parser SemVer
parseSemVer =
  SemVer
    <$> decimal
    <*> (char '.' *> decimal)
    <*> (char '.' *> decimal)
    <*> (char '-' *> some parseNOS <|> mempty)
    <*> (char '+' *> some parseNOS <|> mempty)

ps = parseString

psv = ps parseSemVer mempty

test1 = psv "2.1.1" -- ✅

test2 = psv "1.0.0-x.7.z.92" -- ✅

test3 = psv "1.0.0-gamma+002" -- ✅

test4 = psv "1.0.0-beta+oof.sha.41af286" -- ✅

-- Ord instances
instance Ord NumberOrString where
  compare (NOSS s) (NOSS s') = compare s s'
  compare (NOSI i) (NOSI i') = compare i i'
  compare (NOSS _) (NOSI _) = GT
  compare (NOSI _) (NOSS _) = LT

instance Ord SemVer where
  compare (SemVer mm mn pt re _) (SemVer mm' mn' pt' re' _) = compare mm mm' <> compare mn mn' <> compare pt pt' <> compare re re'
