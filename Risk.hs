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

-- battle :: Battlefield -> Rand StdGen Battlefield

-- ex 2

battle :: Battlefield -> Rand StdGen Battlefield
battle b@(Battlefield a d) =
  do
    let aRollCount = max 0 (a - 1)
        bRollCount = min 2 d
    aRolls <- fmap (reverse . sort) $ replicateM aRollCount getRandom
    bRolls <- fmap (reverse . sort) $ replicateM bRollCount getRandom
    return $ foldl scrap b (zipWith (,) aRolls bRolls)

-- One step of a battle.
scrap :: Battlefield -> (DieValue, DieValue) -> Battlefield
scrap (Battlefield a d) (aRoll, dRoll)
  | dRoll >= aRoll = Battlefield (a - 1) d
  | otherwise = Battlefield a (d - 1)

