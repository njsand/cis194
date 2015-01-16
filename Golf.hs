-- Week 3 homework

module Golf where

import Control.Applicative
import ListHelper

-- Ex 1
skips :: [a] -> [[a]]
skips xs = map ((flip takeNth) xs) [1..length xs]

-- Ex 2
localMaxima :: [Integer] -> [Integer]
localMaxima = map (\(_, x, _) -> x) . filter (\(x,y,z) -> y > x && y > z) . triples

-- Ex 3
