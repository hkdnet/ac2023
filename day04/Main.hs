module Main where

import Data.Map qualified as Map
import System.Environment (getArgs)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language qualified as Lang
import Text.Parsec.Token qualified as TT

type Parser a = Parsec String () a

type Card = (Integer, [Integer], [Integer])

type Input = [Card]

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

solveA :: Input -> Int
solveA cards = sum $ map score cards

score :: Card -> Int
score c =
  if h == 0
    then 0
    else 2 ^ (h - 1)
  where
    h = hitCount c

hitCount :: Card -> Int
hitCount (_, a, b) = length $ filter (`elem` b) a

cardId :: Card -> Integer
cardId (x, _, _) = x

solveB :: Input -> Int
solveB cards = sum $ Map.elems m
  where
    m = solveB' cards (Map.fromList $ map (,1) cardIds)
    cardIds = map cardId cards

solveB' :: [Card] -> Map.Map Integer Int -> Map.Map Integer Int
solveB' [] m = m
solveB' (c@(cardId, _, _) : cs) m =
  if Map.member cardId m
    then solveB' cs newM
    else m
  where
    h = hitCount c
    incrKeys = take h [cardId + 1 ..]
    newM = foldl (mapUpdater (m Map.! cardId)) m incrKeys

mapUpdater :: Int -> Map.Map Integer Int -> Integer -> Map.Map Integer Int
mapUpdater val m key
  | Map.member key m = Map.update (\x -> Just (x + val)) key m
  | otherwise = Map.insert key val m

main :: IO ()
main = do
  args <- getArgs
  str <- readFile ("day04/" ++ head args)
  f $ parse pInput "a" str
  where
    f = either err ok
    ok i = do
      print $ solveB i
    err = print
