-- Find the last but one element of a list.

myButLast :: [a] -> a
myButLast (hd : tl : []) = hd
myButLast (hd : tl)      = myButLast tl
myButLast []             = error "Not Found."

myButLast' :: [a] -> a
myButLast' = head . tail . reverse
