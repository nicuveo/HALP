module Main where

import           Control.Applicative ((<$>))
import           Control.Monad
import           Data.List
import           System.IO
import           Text.Parsec
import           Text.Parsec.String



-- types

type Vector = [Atom]
type Value  = [Int]

data UnaryOperator = Rho
                   deriving (Show, Eq)

data BinaryOperator = Plus
                    | Minus
                    | Product
                    deriving (Show, Eq)

data Operation = B BinaryOperator Vector Expression
               | U UnaryOperator Expression
                deriving (Show, Eq)

data Atom = N Int
          | E Expression
          deriving (Show, Eq)

data Expression = V Vector
                | O Operation
                deriving (Show, Eq)



-- parser helpers

tryAll :: [Parsec String () a] -> Parsec String () a
tryAll parsers = foldl1 (<|>) $ map try parsers

betweenSpaces :: Parser a -> Parser a
betweenSpaces p = spaces *> p <* spaces

betweenParentheses :: Parser a -> Parser a
betweenParentheses p = symbol "(" *> p <* symbol ")"

symbol :: String -> Parsec String () String
symbol s = betweenSpaces $ string s



-- grammar

number :: Parser Int
number = betweenSpaces $ read <$> many1 digit

vector :: Parser Vector
vector = betweenSpaces $ atom `sepBy1` spaces

atom :: Parser Atom
atom = tryAll [ N <$> number
              , E <$> betweenParentheses expr
              ]

unOperator :: Parser UnaryOperator
unOperator = tryAll [ symbol "p" >> return Rho
                    ]

binOperator :: Parser BinaryOperator
binOperator = tryAll [ symbol "+" >> return Plus
                     , symbol "-" >> return Minus
                     , symbol "x" >> return Product
                     ]

operation :: Parser Operation
operation = tryAll [unOp, binOp]
  where binOp = do
          opLeft   <- vector
          binaryOp <- binOperator
          opRight  <- expr
          return $ B binaryOp opLeft opRight
        unOp = do
          unaryOp  <- unOperator
          operand  <- expr
          return $ U unaryOp operand

expr :: Parser Expression
expr = tryAll [ fmap O operation
              , fmap V vector
              ]


-- compute

unFunction :: UnaryOperator -> Value -> Value
unFunction Rho = pure . length

binFunction :: BinaryOperator -> Value -> Value -> Value
binFunction Plus    = zipWith (+)
binFunction Minus   = zipWith (-)
binFunction Product = zipWith (*)

computeAtom :: Atom -> Int
computeAtom (N x) = x
computeAtom (E e) = head $ compute e

compute :: Expression -> Value
compute (V v)         = map computeAtom v
compute (O (U o r))   = unFunction o $ compute r
compute (O (B o l r)) = binFunction o (compute $ V l) $ compute r

eval :: String -> Value
eval = compute . parseWith expr



-- main

parseWith :: Parser a -> String -> a
parseWith p s = either (error . show) id $ parse p "" s

repl :: IO ()
repl = do
  putStr "λ "
  hFlush stdout
  line <- getLine
  unless (null line || nub line == " ") $
    print $ eval line

main :: IO ()
main = forever repl
