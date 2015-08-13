-- Given a run-length code list generated as specified in problem 11. Construct
-- its uncompressed version.

data EncodedElem a = Pair Int a | SingleItem a
  deriving (Show)

decode :: [EncodedElem a] -> [a]
decode lst = concat (map (decompress . toTuple) lst)
  where
    decompress (1, elem) = [elem]
    decompress tpl       = replicate (fst tpl) (snd tpl)
    toTuple (SingleItem elem) = (1, elem)
    toTuple (Pair nb elem)    = (nb, elem)
