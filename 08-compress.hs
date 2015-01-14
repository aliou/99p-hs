-- Eliminate consecutive duplicates of list elements.

compress :: Eq a => [a] -> [a]
compress lst = aux lst (head lst : [])
  where
    aux [] acc        = reverse acc
    aux (hd : tl) acc = do
      if hd == (head acc)
      then aux tl acc
      else aux tl (hd : acc)

