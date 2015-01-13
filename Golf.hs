-- Week 3 homework

module Golf where

import Control.Applicative

import ListHelper

-- Ex 1
skips :: [a] -> [[a]]
skips xs = getZipList $ liftA2 takeNth (ZipList [1..]) (ZipList $ replicate (length xs) xs)

-- Ex 2

localMaxima :: [Integer] -> [Integer]
localMaxima = id

-- Group a list into triples yeah.
triples :: [a] -> [(a,a,a)]
triples xs = getZipList $ liftA3 (,,)
               (ZipList xs) (ZipList $ drop 1 xs) (ZipList $ drop 2 xs)

-- Ex 3
