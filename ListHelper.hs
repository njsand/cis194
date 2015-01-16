module ListHelper
( takeNth,
  takeNth2,
  stitch,
  triples
)
where  

import Control.Applicative

-- Take every @n@th element of a list.
takeNth :: Int -> [a] -> [a]
takeNth n = map snd . filter ((== n) . fst) . zip (cycle [1..n])

-- Take every Nth element from a list and return as a list.  START is the index
-- of the first item to include.
-- E.g.:
-- takeNth 2 0 [1..10] == [1,3,5,7,9]
-- takeNth 2 1 [1..10] == [2,4,6,8,10]
takeNth2 :: Int -> Int -> [a] -> [a]
takeNth2 n start = map snd . filter ((== n) . fst) . 
                  zip (replicate start 0 ++ [n] ++ cycle [1..n])

-- Stitch two lists together.
-- stitch "hlo" "el" == "hello"
stitch x y = concat (zipWith (\a b -> [a, b]) x y)

-- Group a list into triples yeah.
-- Eg.:
-- triples [1,2,3,4] == [(1,2,3), (2,3,4)]
triples :: [a] -> [(a,a,a)]
triples xs = getZipList $ liftA3 (,,)
               (ZipList xs) (ZipList $ drop 1 xs) (ZipList $ drop 2 xs)
