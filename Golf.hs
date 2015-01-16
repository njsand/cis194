-- Week 3 homework

module Golf where

import Control.Applicative
import ListHelper

-- Ex 1
skips :: [a] -> [[a]]
skips xs = getZipList $ liftA2 takeNth (ZipList [1..]) (ZipList $ replicate (length xs) xs)

-- Ex 2



-- Ex 3
