module Format.List.Manipulator (push) where

import Format.List.Token (List (..))

push :: Int -> List -> List -> List
push _ (Nil _) r                  = r
push _ l       (Nil _)            = l
push 0 l (List r t) = Cons (List r t) (Cons l (Nil []) t) t
push 0 l (Cons e (Nil _) t) = Cons e (Cons l (Nil []) t) t
push 0 l (Cons e r t) = Cons e (push 0 l r) t
push i l (List r t) = List (push (i - 1) l r) t
push i l (Cons e r t) = Cons e (push i l r) t
-- push 0 l       (Cons e (Nil _) t) = Cons e (Cons l (Nil []) []) t
-- push 0 l       (Cons e r t)       = Cons e (push 0 l r) t
-- push 0 l       (List r t)         = List (push 0 l r) t
-- push i l       (List r t)         = List (push (i - 1) l r) t
-- push i l       (Cons e r t)       = Cons e (push i l r) t
