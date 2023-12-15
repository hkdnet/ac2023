module Main where

import Data.List
import Data.List.Split (splitOn)
import Data.Map qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Vector qualified as V
import Data.Vector.Unboxed.Base (Vector (V_All))
import System.Environment (getArgs)

type Point = (Int, Int)

type InputMap = (V.Vector (V.Vector Char), Int, Int)

type Input = [InputMap]

type Board = ([Point], [Int], [Int])

parseMap :: String -> Maybe InputMap
parseMap s =
  if null lines
    then Nothing
    else Just (linesToMap, length lines, length $ head lines)
  where
    lines = filter (not . null) $ splitOn "\n" s
    listToMap l = foldl (\acc (i, e) -> Map.insert i e acc) Map.empty $ zip [0 ..] l
    lineToVec line = V.generate (length line) (listToMap line Map.!)
    linesToMap = V.generate (length lines) ((listToMap $ map lineToVec lines) Map.!)

parse :: String -> Input
parse s = mapMaybe parseMap (splitOn "\n\n" s)

pick :: V.Vector (V.Vector Char) -> Int -> Int -> Char
pick v x y = char
  where
    line = (V.!) v x
    char = (V.!) line y

solveMapA :: InputMap -> Int
solveMapA (v, h, w) = head (map (\h -> 100 * (h + 1)) hs ++ map (+ 1) ws)
  where
    hs = filter isHoriSym [0 .. h - 2]
    isHoriSym hIdx = all (\(u, d) -> (V.!) v u == (V.!) v d) $ zip (downToZero hIdx) (upToN (hIdx + 1) (h - 1))
    ws = filter isVertSym [0 .. w - 2]
    fetchCol i = map (\x -> pick v x i) [0 .. h - 1]
    isVertSym wIdx = all (\(u, d) -> fetchCol u == fetchCol d) $ zip (downToZero wIdx) (upToN (wIdx + 1) (w - 1))

downToZero :: Int -> [Int]
downToZero 0 = [0]
downToZero n = n : downToZero (n - 1)

upToN :: (Eq t, Num t) => t -> t -> [t]
upToN from n
  | from == n = [n]
  | otherwise = from : upToN (from + 1) n

solveA i = sum $ map solveMapA i

solveB i = 1

main :: IO ()
main = do
  args <- getArgs
  str <- readFile ("day13/" ++ args !! 1)

  f (head args) $ parse str
  where
    -- either print print $ parse pInput "" test
    f "A" i = print $ solveA i
    f "B" i = print $ solveB i
