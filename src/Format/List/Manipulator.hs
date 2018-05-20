module Format.List.Manipulator (push) where

import Format.List.Token (List (..))

push :: List -> List -> List
push Nil Nil      = Nil
push Nil t        = t
push l   Nil      = l
push (List l) t   = List (push l t)
push (Cons e l) t = Cons e (push l t)
