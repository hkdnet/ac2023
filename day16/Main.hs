module Main where

import Data.Char (ord)
import Data.List
import Data.List.Split (splitOn)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Vector qualified as V
import System.Environment (getArgs)

type Input = [String]

data Tile = Empty | VertSplit | HoriSprit | Slash | BackSlash deriving (Eq, Show)

cToTile '.' = Empty
cToTile '|' = VertSplit
cToTile '-' = HoriSprit
cToTile '/' = Slash
cToTile '\\' = BackSlash

type Board = V.Vector (V.Vector Tile)

parse :: String -> Board
parse s = V.fromList $ filter (not . null) $ map parse' lines
  where
    lines = splitOn "\n" s

parse' :: String -> V.Vector Tile
parse' s = V.fromList $ map cToTile s

solveA i = S.size $ S.map fst $ walk S.empty [((0, 0), E)] i

type Point = (Int, Int)

data Direction = N | E | W | S deriving (Eq, Show, Ord)

delta N = (-1, 0)
delta E = (0, 1)
delta W = (0, -1)
delta S = (1, 0)

add :: Point -> Point -> Point
add (a, b) (c, d) = (a + c, b + d)

walk s [] _ = s
walk s curs b = if s == nextState then s else walk nextState nextCurs b
  where
    nextState = foldl (\acc (p, d) -> S.insert (p, d) acc) s curs
    nextCurs = concatMap (filter (inBoard b) . mapNext) curs
    mapNext (point@(x, y), dir) = mapNext' (pick b x y) point dir

inBoard :: Board -> (Point, Direction) -> Bool
inBoard b ((x, y), _) = x >= 0 && x < h && y >= 0 && y < w
  where
    h = length b
    w = length $ (V.!) b 0

mapNext' t p dir = map (\f -> f p) $ mapFunctions t dir

mapFunctions :: Tile -> Direction -> [Point -> (Point, Direction)]
mapFunctions HoriSprit d
  | d == N || d == S = map stepOne' [E, W]
  | otherwise = [stepOne' d]
mapFunctions VertSplit d
  | d == E || d == W = map stepOne' [N, S]
  | otherwise = [stepOne' d]
mapFunctions Slash d = [stepOne' $ slashDir d]
mapFunctions BackSlash d = [stepOne' $ backslashDir d]
mapFunctions _ d = [stepOne' d]

stepOne' dir p = (add p d, dir)
  where
    d = delta dir

slashDir :: Direction -> Direction
slashDir N = E
slashDir E = N
slashDir S = W
slashDir W = S

backslashDir :: Direction -> Direction
backslashDir N = W
backslashDir W = N
backslashDir S = E
backslashDir E = S

pick board x = (V.!) ((V.!) board x)

-- memoize~~~~
solveB i = maximum $ map (\start -> S.size $ S.map fst $ walk S.empty [start] i) starts
  where
    h = length i
    w = length $ (V.!) i 0
    starts =
      map (\x -> ((x, 0), E)) [0 .. h - 1]
        ++ map (\x -> ((x, w - 1), W)) [0 .. h - 1]
        ++ map (\y -> ((0, y), S)) [0 .. w - 1]
        ++ map (\y -> ((h - 1, y), N)) [0 .. w - 1]

main :: IO ()
main = do
  args <- getArgs
  str <- readFile ("day16/" ++ args !! 1)

  if head args == "A"
    then print $ solveA $ parse str
    else print $ solveB $ parse str
