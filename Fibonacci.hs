-- Hw 6

-- ex 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [1..]

-- ex 2
fibs2 :: [Integer]
fibs2 = [0,1] ++ rec 0 1
 where rec x y = (x + y):rec y (x + y)


data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
  show s = show $ take 20 $ streamToList s

streamToList :: Stream a -> [a]
streamToList (Stream x rest) = x:streamToList rest

-- ex 4
streamRepeat :: a -> Stream a
streamRepeat x = Stream x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x rest) = Stream (f x) $ streamMap f rest
