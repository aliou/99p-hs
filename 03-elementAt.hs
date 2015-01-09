-- Find the K'th element of a list. The first element in the list is number 1.

elementAt :: [a] -> Int -> a
elementAt lst idx
  | idx == 1  = head lst
  | otherwise = elementAt (tail lst) (idx - 1)

elementAt' :: [a] -> Int -> a
