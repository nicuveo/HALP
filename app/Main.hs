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
betweenSpaces = between spaces spaces

betweenParentheses :: Parser a -> Parser a
betweenParentheses = between openParen closeParen
  where openParen  = symbol "("
        closeParen = symbol ")"

symbol :: String -> Parsec String () String
symbol = betweenSpaces . string



-- grammar

number :: Parser Int
number = betweenSpaces $ read <$> many1 digit

vector :: Parser Vector
vector = betweenSpaces $ atom `sepEndBy1` spaces

atom :: Parser Atom
atom = tryAll [ N <$> number
              , E <$> betweenParentheses expr
              ]

unaryOperator :: Parser UnaryOperator
unaryOperator = tryAll [ symbol "p" >> return Rho
                       ]

binaryOperator :: Parser BinaryOperator
binaryOperator = tryAll [ symbol "+" >> return Plus
                        , symbol "-" >> return Minus
                        , symbol "x" >> return Product
                        ]

operation :: Parser Operation
operation = tryAll [unOp, binOp]
  where unOp = do
          unaryOp  <- unaryOperator
          operand  <- expr
          return $ U unaryOp operand
        binOp = do
          opLeft   <- vector
          binaryOp <- binaryOperator
          opRight  <- expr
          return $ B binaryOp opLeft opRight

expr :: Parser Expression
expr = tryAll [ fmap O operation
              , fmap V vector
              ]


-- compute

unaryFunction :: UnaryOperator -> Value -> Value
unaryFunction Rho = pure . length

binaryFunction :: BinaryOperator -> Value -> Value -> Value
binaryFunction Plus    = zipWith (+)
binaryFunction Minus   = zipWith (-)
binaryFunction Product = zipWith (*)

computeAtom :: Atom -> Int
computeAtom (N x) = x
computeAtom (E e) = head $ compute e

compute :: Expression -> Value
compute (V v)         = map computeAtom v
compute (O (U o r))   = unaryFunction o $ compute r
compute (O (B o l r)) = binaryFunction o (compute $ V l) $ compute r

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
  unless (null line || nub line == " ") $ print $ eval line

main :: IO ()
main = forever repl
