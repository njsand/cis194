-- Week 3 homework

module Golf where

import Control.Applicative
import ListHelper
import Data.List

-- Ex 1
skips :: [a] -> [[a]]
skips xs = map ((flip takeNth) xs) [1..length xs]

-- Ex 2
localMaxima :: [Integer] -> [Integer]
localMaxima = map (\(_, x, _) -> x) . filter (\(x,y,z) -> y > x && y > z) . triples

-- Ex 3
histogram :: [Integer] -> String
histogram xs = let counts = map (\x -> length $ filter (== x) xs) [0..9]
                   most = foldl1 max counts
                   lines = map (\x -> map (display . (>= x)) counts) [most, most - 1 .. 0]
               in concat . (intersperse "\n") $ lines ++ 
                  [replicate 10 '='] ++ [concatMap show [0..9]] ++ [[]]

display :: Bool -> Char
display True = '*'
display False = ' '
