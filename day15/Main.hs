module Main where

import Control.Arrow (Arrow (second))
import Data.Char (ord)
import Data.List
import Data.List.Split (divvy)
import System.Environment (getArgs)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language qualified as Lang
import Text.Parsec.Token qualified as TT

type Input = [String]

type Parser a = Parsec String () a

pSpaces :: Parser [Char]
pSpaces = many (char ' ')

pSeq :: Parser [Char]
pSeq = many (noneOf ",\n")

pInput = do
  sepBy pSeq (char ',')

toHash :: String -> Int
toHash = f 0
  where
    f cur [] = cur
    f cur (c : cs) = f ((cur + ord c) * 17 `mod` 256) cs

solveA i = sum $ map toHash i

solveB i = 1

main :: IO ()
main = do
  args <- getArgs
  str <- readFile ("day15/" ++ args !! 1)

  f (head args) $ parse pInput "a" str
  where
    -- either print print $ parse pInput "" test
    f ty = either err (ok ty)
    ok ty i = do
      if ty == "A"
        then print $ solveA i
        else print $ solveB i
    err = print
