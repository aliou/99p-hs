-- Replicate the elements of a list a given number of times.
-- Example:
--  * (repli '(a b c) 3)
-- => (A A A B B B C C C)

repli :: [a] -> Int -> [a]
repli lst nb = concatMap (replicate nb) lst
