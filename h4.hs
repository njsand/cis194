-- Week 4 solutions

import Data.List

-- ex 1
-- This calculates the sum of all even numbers in a list after subtracting
-- 2 from each.
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

-- Does the same as above, but with much more fibre.
fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

-- hailstone sequence
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

step :: Integer -> Integer
step n | even n = n `div` 2
       | otherwise = 3 * n + 1

-- hailstone again
fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate step

-- ex 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
              deriving (Show, Eq)
