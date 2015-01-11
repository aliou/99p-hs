-- Reverse a list.

myReverse :: [a] -> [a]
myReverse lst = aux lst []
  where
    aux (hd : tl) acc = aux tl (hd : acc)
    aux [] acc        = acc
