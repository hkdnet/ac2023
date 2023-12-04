module Main where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language qualified as Lang
import Text.Parsec.Token qualified as TT

type BallOut = (Color, Integer)

type SetInput = [BallOut]

type GameInput = (Integer, [SetInput])

type Input = [GameInput]

type Output = Integer

type Parser a = Parsec String () a

data Color = R | G | B

parserRed :: Parser Color
parserRed = do
  _ <- string "red"
  return R

parserGreen :: Parser Color
parserGreen = do
  _ <- string "green"
  return G

parserBlue :: Parser Color
parserBlue = do
  _ <- string "blue"
  return B

parserColor :: Parser Color
parserColor = parserRed <|> parserGreen <|> parserBlue

parserBallOut :: Parser BallOut
parserBallOut = do
  delta <- parserNatural
  c <- parserColor
  return (c, delta)

parserSet :: Parser SetInput
parserSet = sepBy parserBallOut (string ", ")

parserSets :: Parser [SetInput]
parserSets = sepBy parserSet (string "; ")

parserGame :: Parser GameInput
parserGame = do
  string "Game "
  id <- parserNatural
  char ':'
  char ' '
  sets <- parserSets
  return (id, sets)

parserNatural :: Parser Integer
parserNatural = TT.natural Lang.haskell

parseInput' :: Parser Input
parseInput' = do
  endBy parserGame (char '\n')

parseInput :: String -> Either ParseError Input
parseInput = parse parseInput' "a"

isValidSet :: SetInput -> Bool
isValidSet sets = tr <= 12 && tg <= 13 && tb <= 14
  where
    (tr, tg, tb) = foldl f (0, 0, 0) sets
    f (r, g, b) (R, del) = (r + del, g, b)
    f (r, g, b) (G, del) = (r, g + del, b)
    f (r, g, b) (B, del) = (r, g, b + del)

isValidGame :: GameInput -> Bool
isValidGame (_, sets) = all isValidSet sets

solveA :: Input -> Output
solveA games = sum $ map fst validGames
  where
    validGames = filter isValidGame games

main :: IO ()
main = do
  str <- readFile "a2.in"
  f $ parseInput str
  where
    f = either err ok
    ok i = print $ solveA i
    err = print
