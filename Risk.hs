{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
import Control.Monad

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving (Show)

-- ex 2
battle :: Battlefield -> Rand StdGen Battlefield
battle b@(Battlefield a d) =
  do
    let army1 = max 0 (a - 1)
        army2 = min 2 d
    aRolls <- sortedRolls army1
    bRolls <- sortedRolls army2
    return $ foldl scrap b (zipWith (,) aRolls bRolls)

sortedRolls :: Int -> Rand StdGen [DieValue]
sortedRolls n = fmap (sortBy (flip compare)) $ replicateM n getRandom

-- One step of a battle.
scrap :: Battlefield -> (DieValue, DieValue) -> Battlefield
scrap (Battlefield a d) (attack, defense)
  | attack > defense = Battlefield a (d - 1)
  | otherwise = Battlefield (a - 1) d

-- ex 3
-- Run a battle until its conclusion.
invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield a d)
  | a <= 1 || d == 0 = return b
  | otherwise = do
      result <- battle b
      invade result

-- ex 4
-- Run invade 1000 times and return the probability that the attackers win.
successProb :: Battlefield -> Rand StdGen Double
successProb b@(Battlefield a d) = do 
  battles <- replicateM 1000 (invade b)
  return $ fromIntegral (length $ filter ((== 0) . defenders) battles) / 1000.0

-- Some test definitions
-- b107 = Battlefield 10 7
-- b22 = Battlefield 2 2
-- b11 = Battlefield 1 1
-- b34 = Battlefield 3 4
-- b201 = Battlefield 20 1
-- b205 = Battlefield 20 5
-- b210 = Battlefield 20 10


