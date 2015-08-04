import Control.Applicative

import AParser

-- Questions from end of lecture notes (not homework).
(*>) :: Applicative f => f a -> f b -> f b
fa *> fb = (\x y -> y) <$> fa <*> fb

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA f [] = pure []
mapA f (x:xs) = (:) <$> (f x) <*> mapA f xs

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA 0 x = pure []
-- replicateA n x = (:) <$> x <*> replicateA (n - 1) x
replicateA n x = (:) <$> x <*> replicateA (n - 1) x

-- homework
-- ex 1
oneOrMore :: Parser a -> Parser [a]
oneOrMore x = (:) <$> x <*> zeroOrMore x

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore x = oneOrMore x <|> pure []

-- ex 2
spaces :: Parser String
spaces = zeroOrMore (char ' ')
