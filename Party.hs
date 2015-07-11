module Party where

import Data.Monoid
import Data.Tree
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + empFun e)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL es1 f1) (GL es2 f2) = GL (es1 ++ es2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun x@(GL _ f1) y@(GL _ f2) = if f1 > f2 then x else y

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x children) = f x (map (treeFold f) children)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss@(Emp n f) [] = (GL [boss] f, GL [] 0)
nextLevel boss@(Emp n f) guestLists = (glCons boss $ mconcat $ map snd guestLists,
                                       mconcat $ map fst guestLists)

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel
