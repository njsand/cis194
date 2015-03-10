{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble
(score,
 scoreString,
 Score,
 getScore)
 
where

import Data.Monoid

import qualified Data.Map as Map

newtype Score = Score { getScore :: Int }
  deriving (Eq, Show, Ord, Num)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

scoreMap :: Map.Map Char Int
scoreMap = Map.fromList [
  ('a', 1), 
  ('b', 3),
  ('c', 3),
  ('d', 2),
  ('e', 1),
  ('f', 4),
  ('g', 2),
  ('h', 4),
  ('i', 1),
  ('j', 8),
  ('k', 5),
  ('l', 1),
  ('m', 3),
  ('n', 1),
  ('o', 1),
  ('p', 3),
  ('q', 10),
  ('r', 1),
  ('s', 1),
  ('t', 1),
  ('u', 1),
  ('v', 4),
  ('w', 4),
  ('x', 8),
  ('y', 4),
  ('z', 10)]

score :: Char -> Score
score c = case Map.lookup c scoreMap of
  Just s -> Score s
  Nothing -> Score 0

scoreString :: String -> Score
scoreString = sum . map score
