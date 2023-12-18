module Day08 (part1) where
import Data.List.Split
import Data.List

--data Tree a
--    = Nil
--    | Node a (Tree a) (Tree a)

data Node = Node String String String deriving(Show)
data Move = LeftMove | RightMove deriving(Show,Eq)

parseStringNode :: String -> Node
parseStringNode l = Node currNode leftNode rightNode
    where [currNode, rest] = splitOn " = " l
          [leftNode, rightNode] = splitOn ", " $ (tail . init) rest

parseStringNodes :: [String] -> [Node]
parseStringNodes = map parseStringNode

parseMove :: Char -> Move
parseMove c = case c of
                'L' -> LeftMove
                'R' -> RightMove
                _ -> error (show c)

parseMoves :: String -> [Move]
parseMoves = map parseMove

movesCount :: [Node] -> [Move] -> Int -> String -> String -> Int
movesCount _ [] _ _ _ = 0
movesCount [] _ _ _ _ = 0
movesCount ns ms currMove start end
    | start == end = 0
    | ms !! getCurrMove == LeftMove = 1 + callNextMovesCount getLeft 
    | ms !! getCurrMove == RightMove = 1 + callNextMovesCount getRight   
    where getLeft (Just (Node _ left _)) = left
          getRight (Just (Node _ _ right)) = right
          callNextMovesCount get = movesCount ns ms (getCurrMove+1) (get $ find (\(Node name _ _) -> name==start) ns) end
          getCurrMove = if currMove == length ms then 0 else currMove

part1 = do
    input <- readFile "inputs/day08.txt"
    let [movesString, nodeLines] = (splitOn [""] . lines) input
    let moves = parseMoves $ head movesString
    let nodes = parseStringNodes nodeLines
    print $ movesCount nodes moves 0 "AAA" "ZZZ"
