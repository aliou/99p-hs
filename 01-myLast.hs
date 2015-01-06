-- Find the last element of a list.

myLast :: [a] -> a
myLast (x : []) = x
myLast (x : y)  = myLast y
myLast []       = error "Not Found."

myLast' :: [a] -> a
myLast' = head . reverse
