module Main where

import Data.List
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Ord (Down (Down), comparing)
import Data.Set qualified as S
import System.Environment (getArgs)
import Text.Parsec
  ( Parsec,
    char,
    digit,
    endBy,
    many,
    many1,
    noneOf,
    oneOf,
    parse,
    string,
    try,
    (<|>),
  )
import Text.Parsec.Combinator (sepBy)

type Parser a = Parsec String () a

data Direction = U | D | R | L deriving (Eq, Show, Read)

type Point = (Integer, Integer)

type Input = [LineInput]

type LineInput = (Direction, Integer, String)

nat :: Parser Integer
nat = read <$> many1 digit

pLine :: Parser LineInput
pLine = do
  c <- oneOf "UDRL"
  pSpaces
  len <- nat
  pSpaces
  char '('
  char '#'
  s <- many (oneOf $ ['0' .. '9'] ++ ['a' .. 'f'])
  char ')'
  return (read [c], len, s)

pSpaces :: Parser [Char]
pSpaces = many (char ' ')

pInput :: Parser Input
pInput = do
  endBy pLine (char '\n')

add :: Point -> Point -> Point
add (a, b) (c, d) = (a + c, b + d)

mul :: Integer -> Point -> Point
mul m (a, b) = (a * m, b * m)

dToV U = (-1, 0)
dToV R = (0, 1)
dToV L = (0, -1)
dToV D = (1, 0)

solveA i = S.size $ fill $ dig S.empty (0, 0) i

dig :: S.Set Point -> Point -> [(Direction, Integer, String)] -> S.Set Point
dig s _ [] = s
dig s cur ((d, l, _) : rest) = dig nextSet nextPoint rest
  where
    nextSet = foldl (flip S.insert) s $ map (\m -> add cur $ m `mul` v) [1 .. l]
    nextPoint = add cur (mul l v)
    v = dToV d

fill :: S.Set Point -> S.Set Point
fill s = fill' (S.insert startAt s) startAt
  where
    startAt = add (1, 1) $ S.findMin s

fill' :: S.Set Point -> Point -> S.Set Point
fill' s cur = foldl f s [U, R, L, D]
  where
    f acc d = if S.member nextPoint s then acc else fill' (S.insert nextPoint acc) nextPoint
      where
        nextPoint = add cur $ dToV d

solveB i = 2

main :: IO ()
main = do
  args <- getArgs
  str <- readFile ("day18/" ++ args !! 1)

  f (head args) $ parse pInput "a" str
  where
    -- either print print $ parse pInput "" test
    f ty = either err (ok ty)
    ok ty i = do
      if ty == "A"
        then print $ solveA i
        else print $ solveB i

    err = print

setToBoard :: S.Set Point -> String
setToBoard s = intercalate "\n" $ map buildLine [minX .. maxX]
  where
    xs = S.map fst s
    ys = S.map snd s
    minX = minimum xs
    maxX = maximum xs
    minY = minimum ys
    maxY = maximum ys
    buildLine x = map (\y -> if S.member (x, y) s then '#' else '.') [minY .. maxY]
