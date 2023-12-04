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

extractSymbols :: String -> [Int]
extractSymbols line = idx
  where
    idx = map snd filtered
    filtered = filter onlySymbol indexed
    onlySymbol (c, _) = isSymbol c
    indexed = withIndex line

collectSymbols :: [String] -> [(Int, [Int])]
collectSymbols lines = map f indexed
  where
    f (line, lineNo) = (lineNo, extractSymbols line)
    indexed = withIndex lines

-- solveA :: String -> Int
solveA str = sum $ map ((\(d, _, _) -> d) . fst) filtered
  where
    lines = splitOn '\n' str
    linesWithIndex = withIndex lines
    parsedLines = concatMap f linesWithIndex
      where
        f (l, lineIndex) = map (\numInfo -> (numInfo, lineIndex)) $ parseLine l
    symbols = collectSymbols lines
    filtered = filter (hasAnyAdjucant symbols) parsedLines

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
  print $ solveA str
