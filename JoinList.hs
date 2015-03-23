{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Data.Monoid

import Sized
import Scrabble
import Buffer

import Editor

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
indexJ i _ | i < 0 = Nothing
indexJ i jl | i >= (getSize $ size $ tag jl) = Nothing
indexJ i (Single _ y) = Just y
indexJ i (Append m l r)
  | i < leftSize = indexJ i l
  | otherwise = indexJ (i - leftSize) r
  where leftSize = getSize $ size $ tag l

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl | n <= 0 = jl
dropJ n jl | n >= (getSize $ size $ tag jl) = Empty
dropJ n jl@(Append m l r)
  | n == leftSize = r
  | n < leftSize = dropJ n l +++ r
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

-- Provided
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

-- ex 3
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

balanceJ :: (Sized b, Monoid b) => JoinList b a -> JoinList b a
balanceJ t@(Append m _ _)
  | sz == 1 = t
  | otherwise = (balanceJ $ takeJ hsz t) +++ (balanceJ $ dropJ hsz t)
  where sz = ((getSize . size) m)
        hsz = sz `div` 2
balanceJ t = t

drawJ :: JoinList b a -> String
drawJ jl = "digraph test {\n" ++ (fst $ rec jl 1) ++ "\n}\n" where
  rec (Append m l r) n = 
    let (ltext, lcount) = rec l (n + 1)
        (rtext, rcount) = rec r (lcount + 1)
    in ((show n) ++ " -> " ++ (show $ n + 1) ++ "\n" ++
        (show n) ++ " -> " ++ (show $ lcount + 1) ++ "\n" ++ ltext ++ rtext, rcount)
  rec (Single _ _) n = ("", n)
  rec Empty n = ("", n)
               
instance Buffer (JoinList (Score, Size) String) where
  toString (Append _ l r) = toString l ++ toString r
  toString (Single _ a) = a
  toString Empty = ""

  fromString = balanceJ . foldl (+++) Empty . map (\line -> Single (scoreString line, 1) line) . lines

  line = indexJ

  replaceLine i s b
    | i < 0 || i >= (getSize $ size $ tag b) = b
    | otherwise = takeJ i b +++ (Single (scoreString s, Size 1) s) +++ dropJ (i + 1) b

  numLines = getSize . size . tag

  value = getScore . fst . tag

main = runEditor editor $ (fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ] :: JoinList (Score, Size) String)
