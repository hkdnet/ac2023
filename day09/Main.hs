module Main where

import Control.Arrow (Arrow (second))
import Data.List
import Data.List.Split (divvy)
import System.Environment (getArgs)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language qualified as Lang
import Text.Parsec.Token qualified as TT

type Input = [[Integer]]

type Parser a = Parsec String () a

pSpaces :: Parser [Char]
pSpaces = many (char ' ')

nat :: Parser Integer
nat = read <$> many1 digit

minus :: Parser Integer
minus = do
  char '-'
  val <- nat
  return (val * (-1))

int = minus <|> nat

pLine = do
  sepBy int (char ' ')

pInput = do
  endBy pLine (char '\n')

solveA i = sum $ map solveLineA i

solveLineA line = fillLastElement $ buildSequences line

fillLastElement :: [[Integer]] -> Integer
fillLastElement [a] = head a
fillLastElement (a : b) = last a + fillLastElement b

buildSequences :: [Integer] -> [[Integer]]
buildSequences a =
  if isTerminal a
    then [a]
    else a : buildSequences nx
  where
    nx = diffSeq a

diffSeq :: [Integer] -> [Integer]
diffSeq a = map f $ divvy 2 1 a
  where
    f (a : b : _) = b - a

isTerminal :: [Integer] -> Bool
isTerminal = all (== 0)

solveB :: (Num a) => p -> a
solveB i = 1

main :: IO ()
main = do
  args <- getArgs
  str <- readFile ("day09/" ++ args !! 1)

  f (head args) $ parse pInput "a" str
  where
    -- either print print $ parse pInput "" test
    f ty = either err (ok ty)
    ok ty i = do
      if ty == "A"
        then print $ solveA i
        else print $ solveB i
    err = print
