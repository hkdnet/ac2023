module Main where

import System.Environment (getArgs)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language qualified as Lang
import Text.Parsec.Token qualified as TT

type Parser a = Parsec String () a

type Card = (Integer, [Integer], [Integer])

type Input = [Card]

type Output = Int

pNatural :: Parser Integer
pNatural = TT.natural Lang.haskell

pCard :: Parser Card
pCard = do
  string "Card"
  _ <- pSpaces
  id <- pNatural
  string ":"
  _ <- pSpaces
  winning <- pNumbers
  string "|"
  _ <- pSpaces
  nums <- pNumbers
  return (id, winning, nums)

pSpaces :: Parser [Char]
pSpaces = many (char ' ')

pNumbers :: Parser [Integer]
pNumbers = sepBy1 pNatural (many (char ' '))

pInput :: Parser Input
pInput = do
  ret <- many pCard
  _ <- eof
  return ret

solve :: Input -> Output
solve cards = sum $ map score cards

score :: Card -> Int
score c =
  if h == 0
    then 0
    else 2 ^ (h - 1)
  where
    h = hitCount c

hitCount :: Card -> Int
hitCount (_, a, b) = length $ filter (`elem` b) a

main :: IO ()
main = do
  args <- getArgs
  str <- readFile ("day04/" ++ head args)
  f $ parse pInput "a" str
  where
    f = either err ok
    ok i = do
      print $ solve i
    err = print
