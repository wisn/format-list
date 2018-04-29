module Main where

import Format.List

main :: IO ()
main = print (BOF (
    OpenSQBracket (
        Element 1 (Comma (Element 2 (Comma (
            OpenSQBracket (
                Element 3 (
                    CloseSQBracket (CloseSQBracket (EOF))
                )
            )
        ))))
    )))
