{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s =
    case words s of
        ("I":ts:m) -> LogMessage Info (read ts) (unwords m)
        ("W":ts:m) -> LogMessage Warning (read ts) (unwords m)
        ("E":sev:ts:m) -> LogMessage (Error (read sev)) (read ts) (unwords m)
        _ -> Unknown s

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ t1 _) (Node l tm@(LogMessage _ t2 _) r)
    | t2 > t1 = Node (insert m l) tm r
    | otherwise = Node l tm (insert m r)
insert _ t = t

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

messageString :: LogMessage -> String
messageString (LogMessage _ _ s) = s
messageString (Unknown s) = s

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = messageString <$> filter isValid messages

--insert (Unknown _) mTree = mTree
-- Main
main :: IO ()
main = do
    messages <- testParse parse 6000 "error.log"
    putStrLn "~"
    print $ whatWentWrong $ inOrder $ build messages
