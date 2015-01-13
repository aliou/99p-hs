-- Flatten a nested list structure.

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x)         = [x]
flatten (List (hd : tl)) = flatten hd ++ flatten (List tl)
flatten (List [])        = []
