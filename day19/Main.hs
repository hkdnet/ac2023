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

data Part = X | M | A | S deriving (Eq, Show, Ord)

data Cond = Always | LessThan Part Integer | GreaterThan Part Integer deriving (Eq, Show, Ord)

type Operation = (Cond, Destination)

data Destination = Accept | Reject | Next String deriving (Eq, Show, Ord)

type Parts = (Integer, Integer, Integer, Integer)

type Workflows = [Workflow]

type Input = (Workflows, [Parts])

type Workflow = (String, [Operation])

nat :: Parser Integer
nat = read <$> many1 digit

pSpaces :: Parser [Char]
pSpaces = many (char ' ')

ident :: Parser String
ident = many (oneOf ['a' .. 'z'])

pAccept = do
  char 'A'
  return Accept

pReject = do
  char 'R'
  return Reject

dest :: Parser Destination
dest = do
  pAccept <|> pReject <|> (Next <$> ident)

cToP 'x' = X
cToP 'm' = M
cToP 'a' = A
cToP 's' = S

part = cToP <$> oneOf "xmas"

pOpLessThan = do
  p <- part
  char '<'
  n <- nat
  char ':'
  d <- dest
  return (LessThan p n, d)

pOpGreaterThan = do
  p <- part
  char '>'
  n <- nat
  char ':'
  d <- dest
  return (GreaterThan p n, d)

pOpAlways = do
  d <- dest
  return (Always, d)

pOperation :: Parser Operation
pOperation = do
  try pOpLessThan <|> try pOpGreaterThan <|> pOpAlways

pWorkflow :: Parser Workflow
pWorkflow = do
  name <- ident
  char '{'
  ops <- sepBy pOperation (char ',')
  char '}'
  char '\n'
  return (name, ops)

pParts :: Parser Parts
pParts = do
  _ <- many (noneOf "0123456789")
  x <- nat
  _ <- many (noneOf "0123456789")
  m <- nat
  _ <- many (noneOf "0123456789")
  a <- nat
  _ <- many (noneOf "0123456789")
  s <- nat
  _ <- many (noneOf "0123456789")
  return (x, m, a, s)

pInput :: Parser Input
pInput = do
  workflows <- many pWorkflow
  char '\n'
  partsList <- many pParts
  return (workflows, partsList)

buildWorkflowMap :: Workflows -> M.Map String Workflow
buildWorkflowMap wfs = M.fromList $ zip (map fst wfs) wfs

acceptP :: M.Map String Workflow -> Parts -> Destination -> Bool
acceptP _ _ Accept = True
acceptP _ _ Reject = False
acceptP wm parts (Next cur) = acceptP wm parts dest
  where
    (_, ops) = (M.!) wm cur
    (_, dest) = head $ filter match ops
    match op@(Always, _) = True
    match op@(LessThan t threshold, _) = fetchParts t parts < threshold
    match op@(GreaterThan t threshold, _) = fetchParts t parts > threshold

fetchParts X (x, _, _, _) = x
fetchParts M (_, m, _, _) = m
fetchParts A (_, _, a, _) = a
fetchParts S (_, _, _, s) = s

score (x, m, a, s) = x + m + a + s

solveA (workflows, parts) = sum $ map score $ filter (\p -> acceptP m p (Next "in")) parts
  where
    m = buildWorkflowMap workflows

acceptable :: M.Map String Workflow -> Destination -> [Cond] -> [[Cond]]
acceptable _ Accept ret = [ret]
acceptable _ Reject _ = []
acceptable wm (Next cur) prev = concatMap f $ convertOps ops []
  where
    (_, ops) = (M.!) wm cur
    f ((cond, dest), not) = acceptable wm dest (cond : map negateCond not ++ prev)

convertOps :: [(Cond, Destination)] -> [Cond] -> [((Cond, Destination), [Cond])]
convertOps [] _ = []
convertOps (a@(cond, _) : rest) not = (a, not) : convertOps rest (cond : not)

negateCond (GreaterThan p threshold) = LessThan p (threshold + 1)
negateCond (LessThan p threshold) = GreaterThan p (threshold - 1)

type XmasRange = ((Integer, Integer), (Integer, Integer), (Integer, Integer), (Integer, Integer))

initialRange = ((0, 4001), (0, 4001), (0, 4001), (0, 4001))

countPossibles :: XmasRange -> Integer
countPossibles ((x1, x2), (m1, m2), (a1, a2), (s1, s2)) = if x > 0 && m > 0 && a > 0 && s > 0 then x * m * a * s else 0
  where
    x = x2 - x1 - 1
    m = m2 - m1 - 1
    a = a2 - a1 - 1
    s = s2 - s1 - 1

uniq a = uniq' $ sort a
  where
    uniq' [] = []
    uniq' [a] = [a]
    uniq' (a : b : rest) = if a == b then uniq' (b : rest) else a : uniq' (b : rest)

solveB (workflows, parts) = sum list
  where
    m = buildWorkflowMap workflows
    as = uniq $ acceptable m (Next "in") []
    combs = map (\c -> (c, combinations c as)) [1 .. length as]
    list = map f combs
    f (cnt, condsList) = mul * countSum
      where
        mul = if even cnt then -1 else 1
        countSum = sum $ map ff condsList
        ff conds = countPossibles $ filterOut initialRange (concat conds)

debugRange = ((0, 4), (0, 4), (0, 4), (0, 4))

debug as = (list, sum $ concat list)
  where
    combs = map (\c -> (c, combinations c as)) [1 .. length as]
    list = map f combs
    f (cnt, condsList) = map (* mul) countSum
      where
        mul = if even cnt then -1 else 1
        countSum = map ff condsList
        ff conds = countPossibles $ filterOut debugRange (concat conds)

debug2 (workflows, parts) = map reverse $ acceptable m (Next "in") []
  where
    m = buildWorkflowMap workflows

filterOut :: XmasRange -> [Cond] -> XmasRange
filterOut cur [] = cur
filterOut cur (op : rest) = filterOut nextCur rest
  where
    nextCur = applyFilter cur op

combinations :: Int -> [a] -> [[a]]
combinations n xs = comb n (length xs) xs
  where
    comb 0 _ _ = [[]]
    comb r n a@(x : xs)
      | n == r = [a]
      | otherwise = map (x :) (comb (r - 1) (n - 1) xs) ++ comb r (n - 1) xs

applyFilter ((x1, x2), m, a, s) (GreaterThan X threshold) = ((max x1 threshold, x2), m, a, s)
applyFilter ((x1, x2), m, a, s) (LessThan X threshold) = ((x1, min threshold x2), m, a, s)
applyFilter (x, (m1, m2), a, s) (GreaterThan M threshold) = (x, (max m1 threshold, m2), a, s)
applyFilter (x, (m1, m2), a, s) (LessThan M threshold) = (x, (m1, min threshold m2), a, s)
applyFilter (x, m, (a1, a2), s) (GreaterThan A threshold) = (x, m, (max a1 threshold, a2), s)
applyFilter (x, m, (a1, a2), s) (LessThan A threshold) = (x, m, (a1, min threshold a2), s)
applyFilter (x, m, a, (s1, s2)) (GreaterThan S threshold) = (x, m, a, (max s1 threshold, s2))
applyFilter (x, m, a, (s1, s2)) (LessThan S threshold) = (x, m, a, (s1, min threshold s2))
applyFilter a Always = a

main :: IO ()
main = do
  args <- getArgs
  str <- readFile ("day19/" ++ args !! 1)

  f (head args) $ parse pInput "a" str
  where
    -- either print print $ parse pInput "" test
    f ty = either err (ok ty)
    ok ty i = do
      if ty == "A"
        then print $ solveA i
        else print $ solveB i

    err = print
