{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use first" #-}
module Main where

import Data.Char (digitToInt, isDigit)
import Data.Maybe (catMaybes)
import System.Environment (getArgs)

type NumberInfo = (Int, Int, Int)

cToD :: Char -> Maybe Int
cToD c =
  if isDigit c
    then Just $ digitToInt c
    else Nothing

withIndex :: [a] -> [(a, Int)]
withIndex s = zip s [1 ..]

parseLine :: String -> [NumberInfo]
parseLine = mergeDigits . extractDigits

isSymbol :: Char -> Bool
isSymbol '.' = False
isSymbol c = not $ isDigit c

searchSymbol :: String -> Int -> Int -> Bool
searchSymbol line l r = null range
  where
    range = filter f indexed
    f (c, d) = d >= l && d <= r && isSymbol c
    indexed = withIndex line

mergeDigits :: [(Int, Int)] -> [NumberInfo]
mergeDigits = mergeDigits' []

mergeDigits' :: [(Int, Int, Int)] -> [(Int, Int)] -> [(Int, Int, Int)]
mergeDigits' a [] = a
mergeDigits' [] (x@(d, pos) : xs) = mergeDigits' [(d, pos, pos)] xs
mergeDigits' acc@(last@(lastD, lastStart, lastEnd) : lastRest) (x@(d, pos) : xs) =
  if lastEnd + 1 == pos
    then mergeDigits' ((lastD * 10 + d, lastStart, pos) : lastRest) xs
    else mergeDigits' ((d, pos, pos) : acc) xs

extractDigits :: String -> [(Int, Int)]
extractDigits str = catMaybes maybeList
  where
    maybeList = map f indexed
    f (c, idx) = (\d -> (d, idx)) <$> cToD c
    indexed = withIndex str

extractSymbols :: String -> [(Char, Int)]
extractSymbols line = filter onlySymbol indexed
  where
    onlySymbol (c, _) = isSymbol c
    indexed = withIndex line

collectSymbols :: [String] -> [(Int, [Int])]
collectSymbols lines = map f indexed
  where
    f (line, lineNo) = (lineNo, map snd $ extractSymbols line)
    indexed = withIndex lines

collectGears :: [String] -> [(Int, Int)]
collectGears lines = concatMap f indexed
  where
    f (line, lineNo) = map (\t -> (snd t, lineNo)) $ filter (isGear . fst) $ extractSymbols line
    isGear = (==) '*'
    indexed = withIndex lines

solveA :: String -> Int
solveA str = sum $ map ((\(d, _, _) -> d) . fst) filtered
  where
    lines = splitOn '\n' str
    linesWithIndex = withIndex lines
    parsedLines = concatMap f linesWithIndex
      where
        f (l, lineIndex) = map (\numInfo -> (numInfo, lineIndex)) $ parseLine l
    symbols = collectSymbols lines
    filtered = filter (hasAnyAdjucant symbols) parsedLines

solveB :: String -> Int
solveB str = sum $ map product $ filter (\d -> length d == 2) as
  where
    lines = splitOn '\n' str
    linesWithIndex = withIndex lines
    parsedLines = concatMap f linesWithIndex
      where
        f (l, lineIndex) = map (\numInfo -> (numInfo, lineIndex)) $ parseLine l
    gears = collectGears lines
    as = map (collectAdjucant parsedLines) gears

collectAdjucant :: [(NumberInfo, Int)] -> (Int, Int) -> [Int]
collectAdjucant = f []
  where
    f acc [] gear = acc
    f acc (num@((x, s, e), lineNo) : rest) g@(col, line) =
      if lineNo - 1 <= line && line <= lineNo + 1 && s - 1 <= col && col <= e + 1
        then f (x : acc) rest g
        else f acc rest g

hasAnyAdjucant :: [(Int, [Int])] -> (NumberInfo, Int) -> Bool
hasAnyAdjucant symbols ((d, s, e), numLine) = any f symbols
  where
    f (line, cols) = numLine - 1 <= line && line <= numLine + 1 && any (\c -> s - 1 <= c && c <= e + 1) cols

splitOn :: Char -> String -> [String]
splitOn delimiter = foldr f [[]]
  where
    f c l@(x : xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs

main :: IO ()
main = do
  args <- getArgs
  str <- readFile $ head args
  print $ solveB str
