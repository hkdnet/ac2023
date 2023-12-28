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

solveB i = 1

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
