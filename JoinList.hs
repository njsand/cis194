module JoinList where

import Data.Monoid

import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- ex 1
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append (tag l <> tag r) l r


tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- ex 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ i (Single _ y)
  | i == 0 = Just y
  | otherwise = Nothing
indexJ i (Append m l r)
  | i >= (getSize $ size m) = Nothing
  | i < leftSize = indexJ i l
  | otherwise = indexJ (i - leftSize) r
  where leftSize = getSize $ size $ tag l

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 jl = jl
dropJ _ (Single _ _) = Empty
dropJ n jl@(Append m l r)
  | n >= (getSize $ size m) = Empty
  | n <= leftSize = dropJ n l +++ r
  | n > leftSize = dropJ (n - leftSize) r
  where leftSize = getSize $ size $ tag l

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 _ = Empty
takeJ _ s@(Single _ _) = s
takeJ n a@(Append m l r)
  | n >= (getSize $ size m) = a
  | n <= leftSize = takeJ n l
  | otherwise = l +++ takeJ (n - leftSize) r
  where leftSize = (getSize $ size $ tag l)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

test = Append (Size 4)
         (Append (Size 3)
           (Single (Size 1) 'y')
           (Append (Size 2)
             (Single (Size 1) 'e')
             (Single (Size 1) 'a')))
         (Single (Size 1) 'h')
