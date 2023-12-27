module Main where

import Control.Arrow (Arrow (second))
import Data.Char (ord)
import Data.List
import Data.List.Split (divvy)
import Data.Map qualified as M
import System.Environment (getArgs)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language qualified as Lang
import Text.Parsec.Token qualified as TT

type Input = [String]

type Parser a = Parsec String () a

data Operation = Put String Integer | Remove String deriving (Eq, Show)

nat :: Parser Integer
nat = read <$> many1 digit

pSpaces :: Parser [Char]
pSpaces = many (char ' ')

pSeq :: Parser [Char]
pSeq = many (noneOf ",\n")

pInput = do
  sepBy pSeq (char ',')

opLabel = many (noneOf ",\n=-")

pPut :: Parser Operation
pPut = do
  l <- opLabel
  char '='
  Put l <$> nat

pRemove :: Parser Operation
pRemove = do
  l <- opLabel
  char '-'
  return (Remove l)

pSeqB :: Parser Operation
pSeqB = do
  try pPut <|> pRemove

pInputB = do
  sepBy pSeqB (char ',')

toHash s = fromIntegral $ f 0 s
  where
    f cur [] = cur
    f cur (c : cs) = f ((cur + ord c) * 17 `mod` 256) cs

solveA i = sum $ map toHash i

solveB i = calc $ foldl doOp initBoxState i

calc :: BoxState -> Integer
calc s = sum $ map (calc' . (M.!) s) [0 .. 255]

calc' :: [(String, Integer)] -> Integer
calc' l = sum $ zipWith (curry (\(mul, (s, val)) -> (toHash s + 1) * mul * val)) [1 ..] l

type BoxState = M.Map Integer [(String, Integer)]

doOp :: BoxState -> Operation -> BoxState
doOp s (Remove l) = M.update (removeLens l) key s
  where
    key = toHash l
doOp s (Put l val) = M.update (updateLens l val) key s
  where
    key = toHash l

initBoxState :: BoxState
initBoxState = foldl (\acc key -> M.insert key [] acc) M.empty [0 .. 255]

removeLens :: String -> [(String, Integer)] -> Maybe [(String, Integer)]
removeLens _ [] = Just []
removeLens label (((s, num) : rest)) =
  if s == label
    then removeLens label rest
    else (\a -> (s, num) : a) <$> removeLens label rest

updateLens :: String -> Integer -> [(String, Integer)] -> Maybe [(String, Integer)]
updateLens l v [] = Just [(l, v)]
updateLens l v ((ll, vv) : rest) =
  if l == ll
    then Just ((ll, v) : rest)
    else (\a -> (ll, vv) : a) <$> updateLens l v rest

main :: IO ()
main = do
  args <- getArgs
  str <- readFile ("day15/" ++ args !! 1)

  if head args == "A"
    then f solveA $ parse pInput "a" str
    else f solveB $ parse pInputB "a" str
  where
    -- either print print $ parse pInput "" test
    f solver = either err (ok solver)
    ok solver i = print $ solver i
    err = print
