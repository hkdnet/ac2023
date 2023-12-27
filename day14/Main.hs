module Main where

import Data.List
import Data.List.Split (splitOn)
import Data.Map qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Vector qualified as V
import GHC.Stack.Types (CallStack (EmptyCallStack))
import System.Environment (getArgs)
import Text.Parsec (upper)

data Tile = RoundRock | CubeRock | Empty deriving (Eq, Show)

data Direction = N | E | W | S deriving (Eq, Show)

charToTile :: Char -> Tile
charToTile 'O' = RoundRock
charToTile '#' = CubeRock
charToTile '.' = Empty

type Input = (Tiles, Int, Int)

type Tiles = [[Tile]]

parse :: String -> Input
parse s = (map (map charToTile) lines, length lines, length $ head lines)
  where
    lines = filter (not . null) $ splitOn "\n" s

pick :: [[Tile]] -> Int -> Int -> Tile
pick v 0 0 = head $ head v
pick ((a : r) : rest) 0 y = pick (r : rest) 0 (y - 1)
pick (_ : rest) x y = pick rest (x - 1) y

solveA (m, h, w) = calculate upperRows
  where
    cols = map (\y -> map (\x -> pick m x y) [0 .. h - 1]) [0 .. w - 1]
    uppers = map upperCols cols
    upperRows = transpose uppers

calculate :: [[Tile]] -> Int
calculate rows = sum scores
  where
    height = length rows
    rowsWithIndex = zip rows [height, height - 1 .. 1]
    scores = map f rowsWithIndex
    f (row, mul) = mul * length (filter (== RoundRock) row)

upperCols :: [Tile] -> [Tile]
upperCols [] = []
upperCols (Empty : rest) =
  if isRoundNext rest
    then RoundRock : upperCols (Empty : tailNonEmpty rest 0)
    else Empty : upperCols rest
upperCols (x : rest) = x : upperCols rest

isRoundNext :: [Tile] -> Bool
isRoundNext [] = False
isRoundNext (RoundRock : _) = True
isRoundNext (CubeRock : _) = False
isRoundNext (Empty : rest) = isRoundNext rest

tailNonEmpty :: [Tile] -> Int -> [Tile]
tailNonEmpty [] n = replicate n Empty
tailNonEmpty (Empty : rest) n = tailNonEmpty rest (n + 1)
tailNonEmpty (RoundRock : rest) n = replicate n Empty ++ rest

roundCount = 100

solveB (tiles, h, w) = tileToChars final
  where
    dirs = take (roundCount * 4) $ cycle [N, W, S, E]
    final = foldl tilt tiles dirs

tilt :: Tiles -> Direction -> Tiles
tilt tiles N = transpose $ map upperCols (transpose tiles)
tilt tiles W = map upperCols tiles
tilt tiles S = transpose $ map (reverse . upperCols . reverse) (transpose tiles)
tilt tiles E = map ((reverse . upperCols) . reverse) tiles
  where
    h = length tiles
    w = length (head tiles)

diffCount a b = f a b 0
  where
    f [] [] n = n
    f (x : xs) (y : ys) n = f xs ys (if x == y then n else n + 1)

main :: IO ()
main = do
  args <- getArgs
  str <- readFile ("day14/" ++ args !! 1)

  f (head args) $ parse str
  where
    -- either print print $ parse pInput "" test
    f "A" i = print $ solveA i
    f "B" i = putStr $ solveB i

tileToChars :: Tiles -> String
tileToChars = concatMap ((++ "\n") . f)
  where
    f = map ff
    ff RoundRock = 'O'
    ff CubeRock = '#'
    ff Empty = '.'
