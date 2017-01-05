module Main where

import qualified Token as T
main :: IO ()
main = print $ T.tokenize "(def x 1)"
