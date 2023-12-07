module Main where

import Data.List
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import Text.Parsec
  ( Parsec,
    char,
    endBy1,
    eof,
    many,
    oneOf,
    parse,
    string,
  )
import Text.Parsec.Expr
import Text.Parsec.Language qualified as Lang
import Text.Parsec.Token qualified as TT

type Parser a = Parsec String () a

type Input = ([Integer], [Integer])

type Race = (Integer, Integer)

pNatural :: Parser Integer
pNatural = TT.natural Lang.haskell

pTimeList :: Parser [Integer]
pTimeList = do
  string "Time:"
  pSpaces
  endBy1 pNatural (many (char ' '))

pDistanceList :: Parser [Integer]
pDistanceList = do
  string "Distance:"
  pSpaces
  endBy1 pNatural pSpaces

pSeeds :: Parser [Integer]
pSeeds = do
  _ <- string "seeds: "
  endBy1 pNatural pSpaces

pSpaces :: Parser [Char]
pSpaces = many (char ' ')

pInput :: Parser Input
pInput = do
  t <- pTimeList
  d <- pDistanceList
  return (t, d)

toRaces :: ([Integer], [Integer]) -> [Race]
toRaces (t, d) = zip t d

-- solveA :: Input -> Integer
solveA i = product $ map patternCount races
  where
    races = toRaces i

patternCount :: Race -> Integer
patternCount (t, d) = (half - minPush) * 2 + adjust
  where
    isEven = even t
    adjust = if isEven then 1 else 0
    half = if isEven then t `div` 2 else t `div` 2 + 1
    minPush = fromJust $ minLongerThan d t

minLongerThan :: Integer -> Integer -> Maybe Integer
minLongerThan threshold t = find f [1 ..]
  where
    f p = p * (t - p) > threshold

solveB :: Input -> Integer
solveB _ = 1

main :: IO ()
main = do
  args <- getArgs
  str <- readFile ("day06/" ++ args !! 1)

  if head args == "A"
    then f (head args) $ parse pInput "a" str
    else f (head args) $ parse pInput "a" (filter (/= ' ') str)
  where
    -- either print print $ parse pInput "" test
    f ty = either err (ok ty)
    ok ty i = do
      print $ solveA i
    err = print
