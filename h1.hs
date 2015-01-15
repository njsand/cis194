-- Week 1 homework for http://www.seas.upenn.edu/~cis194/spring13/lectures.html

import ListHelper

-- Ex 1
-- Turn an integer into its digits.
-- Eg., 123 -> [1, 2, 3]
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = (toDigits $ n `div` 10) ++ [n `mod` 10]

-- whatever
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- Ex 2
-- Double every 2nd element of a list.
doubleEveryOther :: [Integer] -> [Integer]

-- My original attempt:
-- doubleEveryOther [] = []
-- doubleEveryOther xs = stitch ((takeNth2 2 0) xs) (map (* 2) $ (takeNth2 2 1) xs)

-- The internet illuminates us:
doubleEveryOther = zipWith ($) (cycle [id, (*2)])

-- Ex 3
sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

-- Ex 4
-- validate CC number
validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigitsRev) n `mod` 10 == 0

-- Ex 5
-- Towers of Hanoi
type Peg = String
type Move = (Peg, Peg)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 src tgt temp = [(src, tgt)]
hanoi n src tgt temp = hanoi (n - 1) src temp tgt ++ [(src, tgt)] ++ hanoi (n - 1) temp tgt src
