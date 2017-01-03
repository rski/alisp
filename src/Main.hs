module Main where

import Data.Char
-- let's tokenize the thing
-- Rather heavily inspired from Milewski's school of haskell articles
data Token = LOperator Operator
           | LIdentifier String
           | LNum Int -- TODO FP?
           | LLParen
           | LRParen
           | LDoubleQuote
           | LSingleQuote
           | LSemicolon -- TODO Newline handling?
           deriving (Show, Eq)

data Operator = Plus | Minus | Mult | Div deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
  | c `elem` "+-*/"  = LOperator (getOp c) : tokenize cs
  | c == '(' = LLParen : tokenize cs
  | c == ')' = LRParen : tokenize cs
  | c == ';' = LSemicolon : tokenize cs
  | isSpace c = tokenize cs
  | isAlpha c = getIdentifier c cs
  | isDigit c = getNum c cs

-- I feel like this would work best with a hash map like thingy
getOp :: Char -> Operator
getOp '+' = Plus
getOp '-' = Minus
getOp '/' = Div
getOp '*' = Mult

getIdentifier :: Char -> String -> [Token]
getIdentifier c cs = let (restIdentifier, rest) = span isAlphaNum cs
                         in LIdentifier (c:restIdentifier) : tokenize rest

getNum :: Char -> String -> [Token]
getNum c cs = let (restNum, rest) = span isDigit cs
                         in LNum (read (c:restNum)) : tokenize rest

main :: IO ()
main = do
  print $ tokenize "(def x 1)"
