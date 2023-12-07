module Main where

import Data.List
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import Text.Parsec
  ( Parsec,
    anyChar,
    char,
    endBy1,
    eof,
    many,
    oneOf,
    parse,
    string,
    try,
    (<|>),
  )
import Text.Parsec.Expr
import Text.Parsec.Language qualified as Lang
import Text.Parsec.Token qualified as TT

type Parser a = Parsec String () a

type Card = Integer

instance Ord Hand where
  compare :: Hand -> Hand -> Ordering
  compare h1 h2 = compare (handKind h1) (handKind h2) <> compareHandRank h1 h2
    where
      compareHandRank (H cs1) (H cs2) = compare cs1 cs2

data HandKind
  = HighCard
  | OnePair
  | TwoPairs
  | ThreeCards
  | FullHouse
  | FourCards
  | FiveCards
  deriving (Eq, Show, Ord)

handKind :: Hand -> HandKind
handKind (H h)
  | length g == 1 = FiveCards
  | length g == 2 = if length (head g) == 4 || length (head g) == 1 then FourCards else FullHouse
  | length g == 3 = if elem 3 $ map length g then ThreeCards else TwoPairs
  | length g == 4 = OnePair
  | otherwise = HighCard
  where
    g = group $ sort h

newtype Hand = H [Card] deriving (Eq, Show)

type Input = [(Hand, Integer)]

cToI :: Char -> Integer
cToI 'A' = 14
cToI 'K' = 13
cToI 'Q' = 12
cToI 'J' = 11
cToI 'T' = 10
cToI '9' = 9
cToI '8' = 8
cToI '7' = 7
cToI '6' = 6
cToI '5' = 5
cToI '4' = 4
cToI '3' = 3
cToI '2' = 2

pNatural :: Parser Integer
pNatural = TT.natural Lang.haskell

pCard :: Parser Integer
pCard = do
  cToI <$> anyChar

pHand :: Parser Hand
pHand = do
  h <- upto 5 pCard
  return (H h)

upto :: Int -> Parser a -> Parser [a]
upto n p | n > 0 = (:) <$> try p <*> upto (n - 1) p <|> return []
upto _ _ = return []

pLine :: Parser (Hand, Integer)
pLine = do
  h <- pHand
  pSpaces
  b <- pNatural
  return (h, b)

pSpaces :: Parser [Char]
pSpaces = many (char ' ')

pInput :: Parser Input
pInput =
  do
    many pLine

-- solveA :: Input -> Integer
solveA i = sum $ zipWith (*) (map (\(_, c, _) -> c) sorted) [1 ..]
  where
    sorted = sortBy f $ map (\(h, b) -> (h, b, handKind h)) i
    f (h1, _, _) (h2, _, _) = compare h1 h2

solveB :: Input -> Integer
solveB _ = 1

main :: IO ()
main = do
  args <- getArgs
  str <- readFile ("day07/" ++ args !! 1)

  if head args == "A"
    then f (head args) $ parse pInput "a" str
    else f (head args) $ parse pInput "a" (filter (/= ' ') str)
  where
    -- either print print $ parse pInput "" test
    f ty = either err (ok ty)
    ok ty i = do
      print $ solveA i
    err = print
