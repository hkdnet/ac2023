module Main where

import Data.List
import Data.Map qualified as Map
import Data.Vector qualified as V
import Data.Vector.Unboxed.Base (Vector (V_All))
import System.Environment (getArgs)

type Point = (Int, Int)

type Input = (V.Vector (V.Vector Char), Int, Int)

type Board = ([Point], [Int], [Int])

parse :: String -> Input
parse s = (linesToMap, length lines, length $ head lines)
  where
    lines = filter (not . null) $ splitOn '\n' s
    listToMap l = foldl (\acc (i, e) -> Map.insert i e acc) Map.empty $ zip [0 ..] l
    lineToVec line = V.generate (length line) (listToMap line Map.!)
    linesToMap = V.generate (length lines) ((listToMap $ map lineToVec lines) Map.!)

splitOn :: Char -> String -> [String]
splitOn delimiter = foldr f [[]]
  where
    f c l@(x : xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs

toBoard :: Input -> Board
toBoard (v, h, w) = (galaxies, emptyRows, emptyCols)
  where
    galaxies = filter (\(x, y) -> pick v x y == '#') (allPoints h w)
    emptyRows = map fst $ filter (\(_, v) -> V.all (== '.') v) $ zip [0 ..] $ map (v V.!) [0 .. h - 1]
    emptyCols = map fst $ filter (\(_, l) -> all (== '.') l) $ zip [0 ..] $ map (\y -> map (\x -> pick v x y) [0 .. h - 1]) [0 .. w - 1]

pick :: V.Vector (V.Vector Char) -> Int -> Int -> Char
pick v x y = char
  where
    line = (V.!) v x
    char = (V.!) line y

allPoints :: Int -> Int -> [(Int, Int)]
allPoints h w = concatMap (zip ws . repeat) hs
  where
    hs = [0 .. h - 1]
    ws = [0 .. w - 1]

solveA i = sum (map (uncurry calcDist) allGalaxyPairs) `div` 2
  where
    (galaxies, emptyRows, emptyCols) = toBoard i
    calcDist (x1, y1) (x2, y2) = calcDistX x1 x2 + calcDistY y1 y2
    calcDistX x1 x2
      | x1 <= x2 = x2 - x1 + length (filter (`elem` emptyRows) [x1 .. x2])
      | otherwise = calcDistX x2 x1
    calcDistY y1 y2
      | y1 <= y2 = y2 - y1 + length (filter (`elem` emptyCols) [y1 .. y2])
      | otherwise = calcDistY y2 y1
    allGalaxyPairs = concatMap (zip galaxies . repeat) galaxies

solveB i = sum (map (uncurry calcDist) allGalaxyPairs) `div` 2
  where
    (galaxies, emptyRows, emptyCols) = toBoard i
    calcDist (x1, y1) (x2, y2) = calcDistX x1 x2 + calcDistY y1 y2
    calcDistX x1 x2
      | x1 <= x2 = x2 - x1 + (1000000 - 1) * length (filter (`elem` emptyRows) [x1 .. x2])
      | otherwise = calcDistX x2 x1
    calcDistY y1 y2
      | y1 <= y2 = y2 - y1 + (1000000 - 1) * length (filter (`elem` emptyCols) [y1 .. y2])
      | otherwise = calcDistY y2 y1
    allGalaxyPairs = concatMap (zip galaxies . repeat) galaxies

main :: IO ()
main = do
  args <- getArgs
  str <- readFile ("day11/" ++ args !! 1)

  f (head args) $ parse str
  where
    -- either print print $ parse pInput "" test
    f "A" i = print $ solveA i
    f "B" i = print $ solveB i
