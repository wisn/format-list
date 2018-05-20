module Format.List.Token
  ( List (..)
  , Token (..)
  )
where

data Token = CloseSquareBracket
           | Comma
           | EOF
           | EOL
           | Literal String
           | OpenSquareBracket
           | Whitespace Int
           deriving (Eq, Show)

data List = Nil
          | List List
          | Cons String List

instance Show List where
  show Nil          = []
  show (List l)     = "List (" ++ show l ++ ")"
  show (Cons e Nil) = "Cons " ++ e
  show (Cons e l)   = "Cons " ++ e ++ ", " ++ show l
