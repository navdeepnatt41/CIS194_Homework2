module LogAnalysis where

import Log 
import Text.Read ( readMaybe )
import qualified Data.Maybe ( isJust )

parseMessage :: String -> LogMessage
parseMessage msg
  | null msg = Unknown []
  | length msg < 3 = Unknown []
  | otherwise =
      case words msg of
        ("I":timeStamp:rest) ->
          if Data.Maybe.isJust (readMaybe timeStamp :: Maybe Int)
            then LogMessage Info (read timeStamp :: Int) (head rest)
            else Unknown msg
        ("W":timeStamp:rest) ->
          if Data.Maybe.isJust (readMaybe timeStamp :: Maybe Int)
            then LogMessage Warning (read timeStamp :: Int) (head rest)
            else Unknown msg
        ("E":lineError:timeStamp:rest) ->
          if Data.Maybe.isJust (readMaybe lineError :: Maybe Int) &&
            Data.Maybe.isJust (readMaybe timeStamp :: Maybe Int)
            then LogMessage (Error (read lineError :: Int)) (read timeStamp :: Int) (head rest)
            else Unknown msg
        _ -> Unknown msg

insert :: LogMessage -> MessageTree -> MessageTree
insert msg@(Unknown m) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ val _) tree@(Node leftTree l@(LogMessage _ timeStamp _) rightTree) 
  | val < timeStamp = Node (insert msg leftTree) l rightTree
  | val > timeStamp = Node leftTree l (insert msg rightTree)
  | otherwise = tree

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left@(Node {}) msg right) = head (inOrder left) : msg : inOrder right

whatWentWrong ::[LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong logs = 
  map (\msg@(LogMessage _ _ str) -> str) 
  $ filter (\msg@(LogMessage _ tS _ ) -> tS >= 50) logs
  