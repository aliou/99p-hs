-- Duplicate the elements of a list.
-- Example:
--  * (dupli '(a b c c d))
-- => (A A B B C C C C D D)

dupli :: [a] -> [a]
dupli lst = aux [] lst
  where
    aux acc []          = acc
    aux acc ( hd : tl ) = aux (acc ++ [hd, hd]) tl
