module Format.List.Token (Token (..)) where

data Token a = BOF (Token a)
             | CloseSQBracket (Token a)
             | Comma (Token a)
             | Element a (Token a)
             | EOF
             | EOL (Token a)
             | OpenSQBracket (Token a)
             | Whitespace (Token a)
             deriving (Eq)

instance Show (Token a) where
    show (BOF t) = "BOF " ++ show t
    show (CloseSQBracket t) = "CloseSQBracket " ++ show t
    show (Comma t) = "Comma " ++ show t
    show (Element _ t) = "Element " ++ show t
    show EOF = "EOF"
    show (EOL t) =
        let token  = calculate t
            total  = length token
            result = if total < 2
                then "EOL "
                else "EOL (" ++ show total ++ ") "
        in result ++ (show. head) token
          where
            calculate :: Token a -> [Token a]
            calculate (EOL t) = calculate t ++ [EOF]
            calculate t       = [t]
    show (OpenSQBracket t) = "OpenSQBracket " ++ show t
    show (Whitespace t) =
        let token  = calculate t
            total  = length token
            result = if total < 2
                then "Whitespace "
                else "Whitespace (" ++ show total ++ ") "
        in result ++ (show. head) token
          where
            calculate :: Token a -> [Token a]
            calculate (Whitespace t) = calculate t ++ [EOF]
            calculate t              = [t]
