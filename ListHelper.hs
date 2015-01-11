module ListHelper
( takeNth,
  stitch
)
where  
  
-- Take every Nth element from a list and return as a list.  START is the index
-- of the first item to include.
-- E.g.:
-- takeNth 2 0 [1..10] == [1,3,5,7,9]
-- takeNth 2 1 [1..10] == [2,4,6,8,10]
takeNth :: Int -> Int -> [a] -> [a]
takeNth n start = map snd . filter ((== n) . fst) . 
                  zip (replicate start 0 ++ [n] ++ cycle [1..n])

-- Stitch two lists together.
-- stitch "hlo" "el" == "hello"
stitch x y = concat (zipWith (\a b -> [a, b]) x y)
