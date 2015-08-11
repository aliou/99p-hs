-- Modify the result of problem P10 in such a way that if an element has no
-- duplicates it is simply copied into the result list. Only elements with
-- duplicates are transferred as (N E) lists.

data EncodedElem = Pair (Int, Char) | SingleItem Char deriving (Show)

toEncodedElem nb elem
  | nb == 1   = SingleItem elem
  | otherwise = Pair (nb, elem)

encode :: [Char] -> [EncodedElem]
encode lst = aux [] (head lst) 1 (tail lst)
  where
    aux acc elem nb []        = reverse ((toEncodedElem nb elem) : acc)
    aux acc elem nb (hd : tl) = do
      if hd == elem
      then aux acc elem (nb + 1) tl
      else aux ((toEncodedElem nb elem) : acc) hd 1 tl

--

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (hd : tl) =
  let (first, rest) = span (== hd) tl in
  (hd : first) : pack rest

encode' :: [Char] -> [EncodedElem]
encode' lst = map aux (pack lst)
  where
    aux tpl = toEncodedElem (length tpl) (head tpl)
