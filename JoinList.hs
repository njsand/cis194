module JoinList where

import Data.Monoid

import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append (tag l <> tag r) l r


tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ i (Single _ y)
  | i == 0 = Just y
  | otherwise = Nothing
indexJ i (Append m l r)
  | i >= (getSize $ size m) = Nothing
  | i < (getSize $ size $ tag l) = indexJ i l
  | otherwise = indexJ (i - (getSize $ size $ tag l)) r
