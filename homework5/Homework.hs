module Homework where
--
import Data.List
--

-- Task 01

{-
  Inspect type signatures of functions that you are supposed to implement
  and and from that information try to think of how Robot and Bearing 
  data types should look like.
-}
data Robot = Robot Bearing Integer Integer deriving Show
data Bearing = North | West | East | South  deriving (Show, Eq)

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot b (x, y) = Robot b x y

bearing :: Robot -> Bearing
bearing (Robot b _ _ ) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ x y) = (x, y)

processCommand :: Robot -> Char -> Robot
processCommand (Robot b x y) command
  | command == 'A' = mkRobot b (advance b (x, y))
  | command == 'R' = mkRobot (turnRight b) (x, y)
  | command == 'L' = mkRobot (turnLeft b) (x, y)

{- It is MANDATORY to implement 'simulate' function in terms of fold -}
simulate :: Robot -> String -> Robot
simulate = foldl processCommand

advance :: Bearing -> (Integer, Integer) -> (Integer, Integer)
advance direction (curX, curY)
  | direction == North = (curX, curY + 1)
  | direction == West  = (curX - 1, curY)
  | direction == South = (curX, curY - 1)
  | direction == East  = (curX + 1, curY)

turnLeft :: Bearing -> Bearing
turnLeft start
  | start == North = West
  | start == West  = South
  | start == South = East
  | start == East  = North 

turnRight :: Bearing -> Bearing
turnRight start
  | start == North = East
  | start == West  = North
  | start == South = West
  | start == East  = South 

-- Task 02

data TriangleType = Equilateral | Isosceles | Scalene | Degenerate | Illegal deriving Show

-- Provide the lengths of triangle sides as input to this function
triangleType :: (Ord a, Num a) => a -> a -> a -> TriangleType
triangleType x y z
  | last sortedBySize > head sortedBySize + sortedBySize !! 1      = Illegal
  | x == y && y == z                                               = Equilateral
  | (x == y && y /= z) || (y == z && x /= z) || (x == z && y/= z)  = Isosceles
  | last sortedBySize == head sortedBySize + sortedBySize !! 1     = Degenerate
  | x /= y && y /= z && x /= z                                     = Scalene
  where sortedBySize = sort [x, y, z]

-- Task 03

{- some convenient test examples -}
-- splitter " > " " > " => ["", ""]
-- splitter " > " "123 > " => ["123", ""]
-- splitter " > " "123 > 456 > 789" => ["123", "456", "789"]

{-
  you don't have to bother with splitting on an empty list e.g.:
  splitter "" "abcde" => ["a", "b", "c", "d", "e"]
-}

{- It is MANDATORY to implement 'splitter' function in terms of fold -}

-- this super complicated beast works as follows:
-- 1. acc is always []
-- 2. accumulate in the accumulator the entire string (otherwise case)
-- 3. p is the incrementing string
-- 4. if p matches the delimeter as a prefix then split
-- 5. split is done by creating a new list (accumulator) with only the
-- current element and, the rest being the current accumulator
-- I'm SORRY (this method took 3 hours to write)
helper :: (Eq a) => [a] -> a -> [[a]] -> [[a]]
helper delim cur all@(p:acc)
  | delim `isPrefixOf` p  = [cur] : all
  | otherwise = (cur : p) : acc

-- this is a bit of a hack since the helper function returns with
-- delimiters
removeDelimiter :: (Eq a) => [a] -> [[a]] -> [[a]]
removeDelimiter delim = map (\x -> if delim `isPrefixOf` x then drop (length delim) x else x)

splitter :: Eq a => [a] -> [a] -> [[a]]
splitter delim xs = removeDelimiter delim (foldr (helper delim) [[]] xs)

-- Task 04

{-
  For this task either write a solution to the problem or if you think
  solution doesn't exist explain why that is the case. Of corse, solution
  must use fold.
-}

-- in general right fold can work with infinite lists
-- that puts left fold out of the question (but putting it below to confirm
-- it won't work)
--
-- the way I would expect a right fold to split a list would be to get to
-- somehow short circuit the whole thing (lazyness)
-- since I don't see that being possible, I can't see how a fold + splitter
-- can work

helperLeft :: (Eq a) => [a] -> [[a]] -> a -> [[a]]
helperLeft delim all@(p:acc) cur 
  | delim `isPrefixOf` p  = [cur] : all
  | otherwise = (cur : p) : acc

splitterInf :: Eq a => [a] -> [a] -> [[a]]
splitterInf delim xs = removeDelimiter delim (foldl (helperLeft delim) [[]] xs)
