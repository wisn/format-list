module Format.List.Parser where

import Format.List.Lexer
import Format.List.Manipulator (push)
import Format.List.Token

data Error = Error Int Int String

instance Show Error where
    show (Error r c m) = m ++ " on row " ++ show r ++ " col " ++ show c ++ "."

data Parser = Parser
            { brackets :: Int
            , col :: Int
            , list :: List
            , row :: Int
            , tokens :: [Token]
            , valid :: Bool
            }

parse :: String -> Either Error List
parse = parseToken. tokenize

parseToken :: [Token] -> Either Error List
parseToken tokens =
    let p        = evaluate tokens
        brackets = getBrackets p
    in if (isValid p) && (brackets == 0)
        then Right (getList p)
        else Left (Error (getRow p) (getCol p) "Parse error")
      where
        evaluate :: [Token] -> Parser
        evaluate t = qStart t initParser

qStart :: [Token] -> Parser -> Parser
qStart [EOF] p
    = fails p
qStart (EOL:xs) p
    = qStart xs $ pushToken EOL (newLine p)
qStart (OpenSquareBracket:xs) p
    = qList xs $ operate parserUpdate
    where
      operate = clearToken. incBrackets
      parserUpdate = pushList (List nil $ getToken p) p
qStart t@((Whitespace _):xs) p
    = qStart xs $ pushToken (head t) (incCol p)
qStart _ p
    = fails p

qList :: [Token] -> Parser -> Parser
qList _ p@(Parser { valid = False })
    = p
qList (CloseSquareBracket:xs) p
    = qEnd xs $ decBrackets p
qList (EOL:xs) p
    = qList xs $ pushToken EOL (newLine p)
qList ((Literal l):xs) p
    = qLit xs $ operate parserUpdate
    where
      operate = clearToken. incCol
      parserUpdate = pushList (Cons (Element l) nil $ getToken p) p
qList (OpenSquareBracket:xs) p
    = qList xs $ operate parserUpdate
    where
      operate = clearToken. incBrackets
      parserUpdate = pushList (List nil $ getToken p) p
qList t@((Whitespace _):xs) p
    = qList xs $ pushToken (head t) (incCol p)
qList _ p
    = fails p

qLit :: [Token] -> Parser -> Parser
qLit (CloseSquareBracket:xs) p
    = qEnd xs $ decBrackets p
qLit (Comma:xs) p
    = qCon xs (incCol p)
qLit (EOL:xs) p
    = qLit xs (newLine p)
qLit ((Whitespace _):xs) p
    = qLit xs (incCol p)
qLit _ p
    = fails p

qCon :: [Token] -> Parser -> Parser
qCon (EOL:xs) p
    = qCon xs (newLine p)
qCon ((Literal l):xs) p
    = qLit xs $ incCol $ pushList (cons (Element l)) p
qCon (OpenSquareBracket:xs) p
    = qList xs $ incBrackets $ pushList (list' nil) p
qCon ((Whitespace _):xs) p
    = qCon xs (incCol p)
qCon _ p
    = fails p

qEnd :: [Token] -> Parser -> Parser
qEnd _ p@(Parser { valid = False })
    = p
qEnd (CloseSquareBracket:xs) p
    = qEnd xs (decBrackets p)
qEnd (Comma:xs) p
    = if (getBrackets p) == 0 then fails p else qCon xs (incCol p)
qEnd [EOF] p
    = setToken (getToken p) p
qEnd (EOL:xs) p
    = qEnd xs (newLine p)
qEnd ((Whitespace _):xs) p
    = qEnd xs (incCol p)
qEnd _ p
    = fails p

getBrackets :: Parser -> Int
getBrackets p@(Parser { brackets = b}) = b

incBrackets :: Parser -> Parser
incBrackets p =
    let brackets = getBrackets p
    in if brackets < 0
        then fails p
        else incCol (p { brackets = brackets + 1 })

decBrackets :: Parser -> Parser
decBrackets p =
    let brackets = getBrackets p
    in if brackets < 0
        then fails p
        else incCol (p { brackets = brackets - 1 })

clearCol :: Parser -> Parser
clearCol p = p { col = 1 }

getCol :: Parser -> Int
getCol (Parser { col = c }) = c

incCol :: Parser -> Parser
incCol p = p { col = (getCol p) + 1 }

getList :: Parser -> List
getList (Parser { list = l }) = l

pushList :: List -> Parser -> Parser
pushList l p = p { list = push (getBrackets p) l (getList p) }

clearRow :: Parser -> Parser
clearRow p = p { row = 1 }

getRow :: Parser -> Int
getRow (Parser { row = r }) = r

incRow :: Parser -> Parser
incRow p = p { row = (getRow p) + 1 }

newLine :: Parser -> Parser
newLine = incRow. clearCol

clearToken :: Parser -> Parser
clearToken p = p { tokens = [] }

getToken :: Parser -> [Token]
getToken (Parser { tokens = t }) = t

pushToken :: Token -> Parser -> Parser
pushToken t p = p { tokens = (getToken p) ++ [t] }

setToken :: [Token] -> Parser -> Parser
setToken t p = p { tokens = t }

isValid :: Parser -> Bool
isValid (Parser { valid = v }) = v

fails :: Parser -> Parser
fails p = p { valid = False }

nil :: List
nil = Nil []

cons :: List -> List
cons l = Cons l nil []

list' :: List -> List
list' l = List l []

initParser :: Parser
initParser = Parser { brackets = 0
                    , col = 1
                    , list = nil
                    , row = 1
                    , tokens = []
                    , valid = True
                    }
