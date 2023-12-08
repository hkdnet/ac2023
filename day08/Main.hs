module Main where

import Data.List
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Ord (Down (Down), comparing)
import Data.Set qualified as Set
import System.Environment (getArgs)
import Text.Parsec
  ( Parsec,
    alphaNum,
    anyChar,
    char,
    choice,
    endBy1,
    eof,
    many,
    oneOf,
    parse,
    string,
    try,
    (<|>),
  )
import Text.Parsec.Combinator (sepBy)
import Text.Parsec.Expr
import Text.Parsec.Language qualified as Lang
import Text.Parsec.Token qualified as TT

type Parser a = Parsec String () a

data Direction = R | L deriving (Eq, Show)

type Point = (String, String, String)

type Input = ([Direction], [Point])

pLeft :: Parser Direction
pLeft = do
  _ <- char 'L'
  return L

pRight = do
  _ <- char 'R'
  return R

pDirection :: Parser Direction
pDirection = pLeft <|> pRight

pDirections :: Parser [Direction]
pDirections = many pDirection

pSpaces :: Parser [Char]
pSpaces = many (char ' ')

pPointName :: Parser String
pPointName = do
  many alphaNum

pPoint :: Parser Point
pPoint = do
  n <- pPointName
  pSpaces
  char '='
  pSpaces
  char '('
  l <- pPointName
  string ", "
  r <- pPointName
  char ')'
  return (n, l, r)

pPoints :: Parser [Point]
pPoints = do
  endBy1 pPoint (char '\n')

pInput :: Parser Input
pInput = do
  ds <- pDirections
  many (char '\n')
  ps <- pPoints
  return (ds, ps)

-- bfs :: Q.Queue String -> Set String -> Points -> Int -> String -> Int
-- bfs _ _ _ step "ZZZ" = step
-- bfs (nx : rest) visited m step cur = bfs nextQueue nextVisited m (step + 1)
--   where
--     nextPoints = m Map.! cur
--     filteredNextPoints = filter (not . (\s -> visited `Set.member` s)) nextPoints
--     nextQueue = foldl (\acc -> \nx -> snoc acc nx) rest filteredNextPoints
--     nextVisited = foldl (\acc -> \nx -> Set.insert acc nx) visited filteredNextPoints

walk :: Map.Map String (String, String) -> Int -> String -> [Direction] -> Int
walk _ step "ZZZ" _ = step
walk m step cur (d : ds) = walk m (step + 1) (nx d) ds
  where
    nx L = l
    nx R = r
    (l, r) = m Map.! cur

solveA (ds, ps) = walk m 0 "AAA" (cycle ds)
  where
    m = Map.fromList $ map (\(s, l, r) -> (s, (l, r))) ps

walkAll :: Map.Map String (String, String) -> Int -> [String] -> [Direction] -> Int
walkAll m step cs (d : ds)
  | all isGoal cs = step
  | otherwise = walkAll m (step + 1) nxs ds
  where
    nxs = map ((\(l, r) -> if d == L then l else r) . (m Map.!)) cs

isStart :: String -> Bool
isStart s = 'A' == last s

isGoal :: String -> Bool
isGoal s = 'Z' == last s

solveB (ds, ps) = walkAll m 0 starts (cycle ds)
  where
    m = Map.fromList $ map (\(s, l, r) -> (s, (l, r))) ps
    starts = filter isStart $ map (\(a, _, _) -> a) ps

main :: IO ()
main = do
  args <- getArgs
  str <- readFile ("day08/" ++ args !! 1)

  f (head args) $ parse pInput "a" str
  where
    -- either print print $ parse pInput "" test
    f ty = either err (ok ty)
    ok ty i = do
      if ty == "A"
        then print $ solveA i
        else print $ solveB i

    err = print
