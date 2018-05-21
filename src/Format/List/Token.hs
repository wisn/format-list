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

data List = Cons List List [Token]
          | Element String
          | List List [Token]
          | Nil [Token]

instance Show List where
  show (Nil _)                       = []
  show (Element e)                   = e
  show (List l _)                    = "List (" ++ show l ++ ")"
  -- show (Cons l@(List _ _) (Nil _) _) = show l
  -- show (Cons l@(List _ _) r _)       = show l ++ ", " ++ show r
  show (Cons l (Nil _) _)            = "Cons " ++ show l
  show (Cons l r _)                  = "Cons " ++ show l ++ ", " ++ show r
