module Chapter24 where

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.Ratio ((%))
import Text.Parser.Combinators
import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

-- Parsing Practice

one :: Parser Char
one = char '1'

one' :: Parser b
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser b
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

testParse' :: String -> Parser String -> IO ()
testParse' s p = print $ parseString p mempty s

testEOF :: Parser () -> IO ()
testEOF p = print $ parseString p mempty "123"

pNL :: String -> IO ()
pNL s = putStrLn ('\n' : s)

-- 2
p123 :: String -> IO ()
p123 s = testParse' s $ choice [string "123", string "12", string "1"]

-- 3
-- write a Parser that does what "string" does, but using "char"
string2 :: String -> Parser String
string2 = traverse char

main :: IO ()
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

ex2 :: Result Integer
ex2 = parseString myParser mempty "123"

ex3 :: Result Integer
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

psv :: String -> Result SemVer
psv = parseString parseSemVer mempty

test1 :: Result SemVer
test1 = psv "2.1.1" -- ✅

test2 :: Result SemVer
test2 = psv "1.0.0-x.7.z.92" -- ✅

test3 :: Result SemVer
test3 = psv "1.0.0-gamma+002" -- ✅

test4 :: Result SemVer
test4 = psv "1.0.0-beta+oof.sha.41af286" -- ✅

-- Ord instances
instance Ord NumberOrString where
  compare (NOSS s) (NOSS s') = compare s s'
  compare (NOSI i) (NOSI i') = compare i i'
  compare (NOSS _) (NOSI _) = GT
  compare (NOSI _) (NOSS _) = LT

instance Ord SemVer where
  compare (SemVer mm mn pt re _) (SemVer mm' mn' pt' re' _) =
    compare mm mm'
      <> compare mn mn'
      <> compare pt pt'
      <> compare re re'

-- 2) Write a parser for positive integer values
parseDigit :: Parser Char
parseDigit = satisfy isDigit

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit <?> "integer"

-- 3) Extend the parser to allow negative integers
base10Integer' :: Parser Integer
base10Integer' = negate <$> (char '-' *> base10Integer) <|> base10Integer

-- 4) parser for US/Canada phones
type NumberingPlanArea = Int

type Exchange = Int

type LineNumber = Int

data PhoneNumber
  = PhoneNumber
      NumberingPlanArea
      Exchange
      LineNumber
  deriving (Eq, Show)

numbers :: (Read b, CharParsing f) => Int -> f b
numbers digits = read <$> count digits digit

parseNumPlan :: Parser NumberingPlanArea
parseNumPlan = optional (string "1-" <|> string "0-") *> numbers 3

parseParenNPlan :: Parser NumberingPlanArea
parseParenNPlan = char '(' *> numbers 3 <* char ')'

parsePhone :: Parser PhoneNumber
parsePhone =
  PhoneNumber
    <$> (parseNumPlan <|> parseParenNPlan)
    <*> (optional (oneOf "- ") *> numbers 3)
    <*> (optional (char '-') *> numbers 4)

phone1 :: Result PhoneNumber
phone1 = parseString parsePhone mempty "123-456-7890"

phone2 :: Result PhoneNumber
phone2 = parseString parsePhone mempty "1234567890"

phone3 :: Result PhoneNumber
phone3 = parseString parsePhone mempty "(123) 456-7890"

phone4 :: Result PhoneNumber
phone4 = parseString parsePhone mempty "1-123-456-7890"
