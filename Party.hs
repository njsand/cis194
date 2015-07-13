module Party where

import Data.Monoid
import Data.Tree
import Employee

-- Ex 1.1
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + empFun e)

-- Ex 1.2
instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL es1 f1) (GL es2 f2) = GL (es1 ++ es2) (f1 + f2)

-- Ex 1.3
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Ex 2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x children) = f x (map (treeFold f) children)

-- Ex 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss@(Emp n f) [] = (GL [boss] f, GL [] 0)
nextLevel boss@(Emp n f) guestLists =
  let nobosses = mconcat $ map snd guestLists
      bosses = mconcat $ map fst guestLists
  in (glCons boss nobosses,
      moreFun bosses nobosses)

-- Ex 4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- Ex 5
main :: IO ()
main = do
  company <- fmap read (readFile "company.txt")
  let (GL es f) = maxFun company
  putStrLn $ "Total fun: " ++ show f
  putStr $ (unlines . map empName) es
