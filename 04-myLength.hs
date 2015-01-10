-- Find the number of elements of a list.

myLength :: [a] -> Int
myLength []        = 0
myLength (_ : tl) = 1 + myLength tl
