{-# OPTIONS_GHC -Wall #-}

-- Week 2 homework for http://www.seas.upenn.edu/~cis194/spring13/lectures.html

module LogAnalysis where

import Log
import Data.Char(isSpace)

-- Ex 1
parseMessage :: String -> LogMessage
parseMessage (xs) = case take 3 $ words xs of
  ["E", level, timestamp] -> LogMessage (Error $ read level) (read timestamp) (messageString 3 xs)
  ["I", timestamp, _] -> LogMessage Info (read timestamp) (messageString 2 xs)
  ["W", timestamp, _] -> LogMessage Warning (read timestamp) (messageString 2 xs)
  _ -> Unknown xs

messageString :: Int -> String -> String
messageString n text = (!! n) $ iterate ((dropWhile isSpace) . (dropWhile (not . isSpace))) text

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Ex 2
insert :: LogMessage -> MessageTree -> MessageTree

insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert logmsg@(LogMessage _ stamp _) (Node left current@(LogMessage _ stamp2 _) right)
  | stamp <= stamp2 = (Node (insert logmsg left) current right)
  | stamp > stamp2 = (Node left current (insert logmsg right))
  | otherwise = error "should not happen"

insert _ (Node _ (Unknown _) _) = error "what"
-- Now why do I need this?  Do not understand.
-- insert _ (Node _ (LogMessage _ _ _) _) = error "what"  

-- Ex 3
build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

-- Ex 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logmsg@(LogMessage _ _ _) right) = inOrder left ++ [logmsg] ++ inOrder right
inOrder (Node _ (Unknown _) _) = error "should not happen wtf"

-- Ex 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map (\(LogMessage _ _ msg) -> msg) . filter needLog . inOrder . build

needLog :: LogMessage -> Bool
needLog (LogMessage (Error level) _ _) = level >= 50
needLog _ = False
