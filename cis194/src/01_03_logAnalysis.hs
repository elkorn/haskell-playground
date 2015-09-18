{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Control.Applicative

main =
  do msgs <-
       parse <$>
       readFile "error.log"
     print $
       (length . inOrder . build $ msgs) ==
       length (filter isKnown msgs)

isKnown :: LogMessage -> Bool
isKnown (LogMessage _ _ _) = True
isKnown _ = False

parseMessage :: String -> LogMessage
parseMessage str =
  let words' = words str
      header = take 3 words'
      stdMsg :: MessageType -> String -> LogMessage
      stdMsg type' timestamp =
        LogMessage
          type'
          (read timestamp)
          (unwords $
           drop 2 words')
  in case header of
       ["E",severity,timestamp] ->
         (LogMessage
            (Error $ read severity)
            (read timestamp)
            (unwords $
             drop 3 words'))
       ["I",timestamp,_] -> stdMsg Info timestamp
       ["W",timestamp,_] ->
         stdMsg Warning timestamp
       _ -> Unknown str

parse :: String -> [LogMessage]
parse =
  (map parseMessage) .
  lines

getTimestamp :: LogMessage -> Int
getTimestamp (LogMessage _ ts _) = ts
getTimestamp (Unknown _) = -1

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert msg Leaf = Node Leaf msg Leaf
insert (LogMessage t ts c) (Node l msg r)
  | ts <= getTimestamp msg =
    Node (insert (LogMessage t ts c) l) msg r
  | otherwise =
    Node l msg (insert (LogMessage t ts c) r)
build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

build' :: [LogMessage] -> Int
build' = foldl (\t _ -> t + 1) 0

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l msg r) =
  inOrder l ++
  [msg] ++
  inOrder r
