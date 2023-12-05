module Main where

import Data.List
import Data.Map qualified as Map
import System.Environment (getArgs)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language qualified as Lang
import Text.Parsec.Token qualified as TT

type Parser a = Parsec String () a

type Card = (Integer, [Integer], [Integer])

type Input = ([Integer], [[MapInput]])

type MapInput = (Integer, Integer, Integer)

type MapResolver = Integer -> Integer

buildMapResolver :: [MapInput] -> Integer -> Integer
buildMapResolver ranges src = f optRange
  where
    f (Just (dstStart, srcStart, _)) = dstStart + (src - srcStart)
    f Nothing = src
    optRange = find (containRange src) ranges

containRange :: Integer -> MapInput -> Bool
containRange srcInput (dstStart, srcStart, len) = srcStart <= srcInput && srcInput <= srcStart + len

pNatural :: Parser Integer
pNatural = TT.natural Lang.haskell

pSeeds :: Parser [Integer]
pSeeds = do
  _ <- string "seeds: "
  endBy1 pNatural (many (char ' '))

pMapInput :: Parser MapInput
pMapInput = do
  a <- pNatural
  pSpaces
  b <- pNatural
  pSpaces
  c <- pNatural
  return (a, b, c)

pAlphabet :: Parser Char
pAlphabet = do
  oneOf ['a' .. 'z']

pMap :: Parser [MapInput]
pMap = do
  many pAlphabet
  string "-"
  many pAlphabet
  string "-"
  many pAlphabet
  pSpaces
  string "map:"
  string "\n"
  many pMapInput

pSpaces :: Parser [Char]
pSpaces = many (char ' ')

pInput :: Parser Input
pInput = do
  seeds <- pSeeds
  mapInputs <- many pMap
  _ <- eof
  return (seeds, mapInputs)

solveA :: Input -> Integer
solveA (seeds, mapSources) = minimum locations
  where
    maps = map buildMapResolver mapSources
    resolve seed = foldl (\acc resolver -> resolver acc) seed maps
    locations = map resolve seeds

solveB :: Input -> Integer
solveB (seedInput, mapSources) = minimum locations
  where
    seeds = eachPair seedInput
    maps = map buildMapResolver mapSources
    resolve seed = foldl (\acc resolver -> resolver acc) seed maps
    locations = concatMap (\(start, len) -> map resolve (take (fromIntegral len) [start, start + 1 ..])) seeds

eachPair :: [a] -> [(a, a)]
eachPair [] = []
eachPair (a : b : rest) = (a, b) : eachPair rest
eachPair _ = error "!?"

main :: IO ()
main = do
  args <- getArgs
  str <- readFile ("day05/" ++ args !! 1)

  -- either print print $ parse pInput "" test

  f (head args) $ parse pInput "a" str
  where
    f ty = either err (ok ty)
    ok ty i = do
      if ty == "A"
        then print $ solveA i
        else print $ solveB i
    err = print
