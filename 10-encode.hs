-- Run-length encoding of a list. Use the result of problem P09 to implement the
-- so-called run-length encoding data compression method. Consecutive duplicates
-- of elements are encoded as lists (N E) where N is the number of duplicates
-- of the element E.

encode :: Num b => [Char] -> [(b, Char)]
encode lst = aux [] (head lst) 1 (tail lst)
  where
    aux acc elem nb []        = reverse ((nb, elem) : acc)
    aux acc elem nb (hd : tl) = do
      if hd == elem
      then aux acc elem (nb + 1) tl
      else aux ((nb, elem) : acc) hd 1 tl
