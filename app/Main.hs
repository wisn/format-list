module Main where

import Format.List

main :: IO ()
main = do
    (print. tokenize) "[1,2, [3,\n  [4]], 5]"
