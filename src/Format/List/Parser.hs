module Format.List.Parser where

import Format.List.Lexer
import Format.List.Manipulator (push)
import Format.List.Token

data Error = Error Int Int String

instance Show Error where
    show (Error r c m) = m ++ " on row " ++ show r ++ " col " ++ show c ++ "."

data Parser = Parser
            { col :: Int
            , list :: List
            , row :: Int
            , valid :: Bool
            }

parse :: String -> Either Error List
parse = parseToken. tokenize

parseToken :: [Token] -> Either Error List
parseToken tokens =
    let p = evaluate tokens
    in if (isValid p)
        then Right (getList p)
        else Left (Error (getRow p) (getCol p) "Parse error")
      where
        evaluate :: [Token] -> Parser
        evaluate t = qStart t initParser

qStart :: [Token] -> Parser -> Parser
qStart [EOF]                    p = (fails. clearCol. clearRow) p
qStart (EOL:xs)                 p = qStart xs (incRow p)
qStart (OpenSquareBracket:xs)   p = qList xs (pushList (incCol p) (List Nil))
qStart ((Whitespace _):xs)      p = qStart xs p
qStart _                        p = fails p

qList :: [Token] -> Parser -> Parser
qList (CloseSquareBracket:xs) p = qEnd xs (incCol p)
qList (EOL:xs)                p = qList xs (incRow p)
qList ((Literal l):xs)        p = qLit xs (pushList (incCol p) (Cons l Nil))
qList (OpenSquareBracket:xs)  p = qList xs (incCol p)
qList ((Whitespace _):xs)     p = qList xs (incCol p)
qList _                       p = fails p

qLit :: [Token] -> Parser -> Parser
qLit (CloseSquareBracket:xs) p = qEnd xs (incCol p)
qLit (Comma:xs)              p = qCon xs (incCol p)
qLit (EOL:xs)                p = qLit xs (incRow p)
qLit ((Whitespace _):xs)     p = qLit xs (incCol p)
qLit _                       p = fails p

qCon :: [Token] -> Parser -> Parser
qCon (EOL:xs)               p = qCon xs (incRow p)
qCon ((Literal l):xs)       p = qLit xs (pushList (incCol p) (Cons l Nil))
qCon (OpenSquareBracket:xs) p = qList xs (pushList (incCol p) (List Nil))
qCon ((Whitespace _):xs)    p = qCon xs (incCol p)
qCon _                      p = fails p

qEnd :: [Token] -> Parser -> Parser
qEnd (CloseSquareBracket:xs) p = qEnd xs (incCol p)
qEnd (Comma:xs)              p = qCon xs (incCol p)
qEnd [EOF]                   p = p
qEnd (EOL:xs)                p = qEnd xs (incRow p)
qEnd ((Whitespace _):xs)     p = qEnd xs (incCol p)
qEnd _                       p = fails p

clearCol :: Parser -> Parser
clearCol p = p { col = 0 }

getCol :: Parser -> Int
getCol (Parser { col = c }) = c

incCol :: Parser -> Parser
incCol p = p { col = (getCol p) + 1 }

getList :: Parser -> List
getList (Parser { list = l }) = l

pushList :: Parser -> List -> Parser
pushList p t = p { list = push (getList p) t }

clearRow :: Parser -> Parser
clearRow p = p { row = 0 }

getRow :: Parser -> Int
getRow (Parser { row = r }) = r

incRow :: Parser -> Parser
incRow p = p { row = (getRow p) + 1 }

isValid :: Parser -> Bool
isValid (Parser { valid = v }) = v

fails :: Parser -> Parser
fails p = p { valid = False }

initParser :: Parser
initParser = Parser { col = 1, list = Nil, row = 1, valid = True }
