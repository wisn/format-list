module Format.List.Lexer (tokenize) where

import Format.List.Token (Token (..))

language :: String
language = "[,] \n"

tokenize :: String -> [Token]
tokenize []        = [EOF]
tokenize (',':xs)  = Comma : tokenize xs
tokenize ('\n':xs) = EOL : tokenize xs
tokenize ('[':xs)  = OpenSquareBracket : tokenize xs
tokenize (']':xs)  = CloseSquareBracket : tokenize xs
tokenize (' ':xs)  = Whitespace n : tokenize ys
  where
    evaluate :: Int -> String -> (Int, String)
    evaluate x (' ':xs) = evaluate (x + 1) xs
    evaluate x xs       = (x, xs)
    (n, ys)             = evaluate 1 xs
tokenize xs        = Literal e : tokenize ys
  where
    evaluate :: String -> String -> (String, String)
    evaluate e (x:xs)
        | elem x language = (e, [x] ++ xs)
        | otherwise       = evaluate (e ++ [x]) xs
    (e, ys) = evaluate [] xs
