-- Drop every N'th element from a list.

dropEvery :: [a] -> Int -> [a]
dropEvery lst n = aux [] lst n
  where
    aux acc [] n          = acc
    aux acc ( hd : tl ) 1 = aux acc tl n
    aux acc ( hd : tl ) n = aux ( acc ++ [hd] ) tl (n - 1)
