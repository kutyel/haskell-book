module Chapter24 where

import Control.Applicative ((<|>))
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Char (digitToInt, isDigit)
import Data.List (intercalate, intersperse)
import Data.Ratio ((%))
import Data.Word
import Numeric (showHex)
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

numbers :: (Read a, CharParsing f) => Int -> f a
numbers digits = read <$> count digits digit

parsePhone :: Parser PhoneNumber
parsePhone =
  PhoneNumber
    <$> (optional (string "1-") *> numbers 3 <|> char '(' *> numbers 3 <* char ')')
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

-- 5) skipped

-- 6) parser for IPv4 addresses

newtype IPAddress
  = IPAddress Word32
  deriving (Eq, Ord)

fromOctetsToWord32 :: Integer -> Integer -> Integer -> Integer -> Word32
fromOctetsToWord32 a b c d =
  fromIntegral $
    shiftL a 24
      .|. shiftL b 16
      .|. shiftL c 8
      .|. d

parseWord32 :: Parser Word32
parseWord32 =
  fromOctetsToWord32
    <$> (integer <* char '.')
    <*> (integer <* char '.')
    <*> (integer <* char '.')
    <*> integer

parseIPv4 :: Parser IPAddress
parseIPv4 = IPAddress <$> parseWord32

ip1 :: Result IPAddress
ip1 = parseString parseIPv4 mempty "172.16.254.1" -- 2886794753

ip2 :: Result IPAddress
ip2 = parseString parseIPv4 mempty "204.120.0.15" -- 3430416399

-- 7) parser for IPv6 addresses

data IPAddress6
  = IPAddress6 Word64 Word64
  deriving (Eq, Ord)

createIPv6 :: [Word16] -> IPAddress6
createIPv6 xs = IPAddress6 part1 part2
  where
    (h, t) = splitAt 4 (take 8 (xs ++ repeat 0))
    part1 = fromIntegral $ combineBitBlocks 16 h
    part2 = fromIntegral $ combineBitBlocks 16 t

combineBitBlocks :: Integral a => Int -> [a] -> Integer
combineBitBlocks x = foldl (\acc d -> fromIntegral d + shiftL acc x) 0

hexToInteger :: String -> Integer
hexToInteger = foldl (\acc d -> 16 * acc + fromIntegral (digitToInt d)) 0

parseHextet :: Parser Word16
parseHextet = do
  hex <- count 4 hexDigit -- this only parses fully expanded IPv6
  let val = hexToInteger hex
  return $ fromIntegral val

parseWords16 :: Parser [Word16]
parseWords16 = (:) <$> parseHextet <*> count 7 (char ':' *> parseHextet)

parseIPv6 :: Parser IPAddress6
parseIPv6 = createIPv6 <$> parseWords16

ip3 :: Result IPAddress6
ip3 = parseString parseIPv6 mempty "0000:0000:0000:0000:0000:ffff:ac10:fe01" -- == ip1

ip4 :: Result IPAddress6
ip4 = parseString parseIPv6 mempty "0000:0000:0000:0000:0000:ffff:cc78:f000" -- == ip2

ip5 :: Result IPAddress6
ip5 = parseString parseIPv6 mempty "fe80:0000:0000:0000:0202:b3ff:fe1e:8329" -- wrong...

-- 8) write your own Show instance for IPAddress and IPAddress6

splitBitBlocks :: Integral a => Int -> Int -> a -> [Integer]
splitBitBlocks n b dec = (reverse . map fst . take n . drop 1) bls
  where
    bls = iterate (\(_, d) -> (d .&. (2 ^ b - 1), shiftR d b)) (0, fromIntegral dec)

instance Show IPAddress where
  show (IPAddress n) = intercalate "." (map show $ splitBitBlocks 4 8 n)

instance Show IPAddress6 where
  show (IPAddress6 n1 n2) = (foldr ($) "" . intersperse (":" ++) . map showHex) hs
    where
      hs = splitBitBlocks 4 16 n1 ++ splitBitBlocks 4 16 n2

-- 9) write a function that converts between IPAddress and IPAddress6

ipV4toV6 :: IPAddress -> IPAddress6
ipV4toV6 (IPAddress ip) = IPAddress6 0 (shiftL (2 ^ 16 - 1) 32 .|. fromIntegral ip)
-- 10) parser for DOT language -- SKIPPED
