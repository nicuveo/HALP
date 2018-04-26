module Main where

import           Control.Applicative ((<$>))
import           Control.Monad
import           Data.List
import           System.IO
import           Text.Parsec
import           Text.Parsec.String



-- types

type Vector = [Int]

data Operator = Plus
              | Minus
              | Product
              deriving (Show, Eq)

type Operation = (Operator, Vector, Expression)

data Expression = V Vector
                | O Operation
                deriving (Show, Eq)



-- parser helpers

tryAll :: [Parsec String () a] -> Parsec String () a
tryAll parsers = foldl1 (<|>) $ map try parsers

betweenSpaces :: Parser a -> Parser a
betweenSpaces p = spaces *> p <* spaces

symbol :: String -> Parsec String () String
symbol s = betweenSpaces $ string s



-- grammar

number :: Parser Int
number = betweenSpaces $ read <$> many1 digit

vector :: Parser Vector
vector = betweenSpaces $ number `sepBy1` spaces

operator :: Parser Operator
operator = tryAll [ symbol "+" >> return Plus
                  , symbol "-" >> return Minus
                  , symbol "x" >> return Product
                  ]

operation :: Parser Operation
operation = do
  opLeft   <- vector
  binaryOp <- operator
  opRight  <- expr
  return (binaryOp, opLeft, opRight)

expr :: Parser Expression
expr = tryAll [ fmap O operation
              , fmap V vector
              ]


-- compute

function :: Operator -> Vector -> Vector -> Vector
function Plus    = zipWith (+)
function Minus   = zipWith (-)
function Product = zipWith (*)

compute :: Expression -> Vector
compute (V v) = v
compute (O (o, l, r)) = function o l $ compute r

eval :: String -> Vector
eval line = compute $ either (error . show) id $ parse expr "" line



-- main

repl :: IO ()
repl = do
  putStr "λ "
  hFlush stdout
  line <- getLine
  unless (null line || nub line == " ") $
    print $ eval line

main :: IO ()
main = forever repl
