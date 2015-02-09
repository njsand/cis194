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
  show s = show $ take 50 $ streamToList s

streamToList :: Stream a -> [a]
streamToList (Stream x rest) = x:streamToList rest

-- ex 4
streamRepeat :: a -> Stream a
streamRepeat x = Stream x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x rest) = Stream (f x) $ streamMap f rest

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x $ streamFromSeed f (f x)

-- ex 5
-- |A stream version of [1,2..]
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- |Alternates the elements from two streams.
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x s) s2 = Stream x $ interleaveStreams s2 s
                                        
-- |The ruler function.
ruler :: Stream Integer
ruler = rec 0 where
 rec n = interleaveStreams (streamRepeat n) (rec $ 1 + n)

