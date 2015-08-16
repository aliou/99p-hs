-- Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.

split :: [a] -> Int -> ([a], [a])
split [] _   = ([], [])
split lst nb
  | nb < 0 = ([], [])
  | otherwise =  aux [] lst nb
    where
      aux acc lst 0          = (acc, lst)
      aux acc ( hd : tl ) nb = aux (acc ++ [hd]) tl (nb - 1)
