-- Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate
-- sublists.

pack :: Eq a => [a] -> [[a]]
pack lst = aux [] (head lst : []) (tail lst)
  where
    aux subl cur []        = reverse (cur : subl)
    aux subl cur (hd : tl) = do
      if hd == (head cur)
      then aux subl (hd : cur) tl
      else aux (cur : subl) (hd : []) tl

pack' :: Eq a => [a] -> [[a]]
pack' (hd : tl) =
  let (first, rest) = span (== hd) tl in
  (hd : first) : pack rest

pack' [] = []
