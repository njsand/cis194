-- Week 3 homework

module Golf where

-- import Control.Applicative
import ListHelper
import Data.List

-- Ex 1
skips :: [a] -> [[a]]
skips xs = map ((flip takeNth) xs) [1..length xs]

-- Ex 2
localMaxima :: [Integer] -> [Integer]
localMaxima = map (\(_, x, _) -> x) . filter (\(x,y,z) -> y > x && y > z) . triples

-- Ex 3
-- Draw a frequency histogram of a list of numbers that all lie in the range [0..9].
histogram :: [Integer] -> String
histogram xs = let counts = map (length . (flip filter $ xs) . (==)) [0..9]
                   height = foldl1 max counts
                   lines = map (\x -> map (display . (>= x)) counts) [height, height - 1 .. 1]
               in unlines $ lines ++ [replicate 10 '='] ++ [concatMap show [0..9]]

display :: Bool -> Char
display True = '*'
display False = ' '
