module Homework where
--
import Data.List
import Data.Char
import Data.Bits ( xor )
--

-- Task 01
localMaxima :: [Int] -> [Int]
-- Invoke isLocalMaximum for every element of the list not first and last
-- putting conditions of first/last before function invocation
localMaxima xs = [x | (ind, x) <- zip [0..] xs, ind > 0, ind < length xs - 1, isLocalMaximum ind xs]
isLocalMaximum :: Int -> [Int] -> Bool
isLocalMaximum index xs = biggerThanPrev && biggerThanNext
    where biggerThanPrev = (xs !! index) > (xs !! (index - 1))
          biggerThanNext = (xs !! index) > (xs !! (index + 1))

-- Task 02
transform :: [(Int, String)] -> [(Char, Int)]
transform [] = []                        
transform ((x, y):xs) = [(toLower c, x) | c <- y] ++ transform xs

-- Task 03
rule90 :: [Bool] -> [[Bool]]
rule90 xs = xs : rule90 iterResult
    where iterResult = rule90Step xs

-- the task says the assumption is that edges are False, 
-- but Wiki states they are results of their neighbors
-- best illustrated by https://en.wikipedia.org/wiki/File:Rule_90_gate_array.svg
-- so using ugly hardcoding to take the second (for the first) and second to last element (for the last)
rule90Step :: [Bool] -> [Bool]
rule90Step xs = [xs !! 1] ++ rul' xs ++ [xs !! (length xs - 2)] 
        where rul' (a:b:c:xs) = xor a c : rul' (b : c : xs)
              -- when I have two elements return empty since that's an edge
              rul' (a:b:xs) = []
              -- required only when submitting an empty list as a start
              -- argument
              rul' [] = []
-- rules
--     | xs == [True, True, True]     = [False]
--     | xs == [True, True, False]    = [True]
--     | xs == [True, False, True]    = [False]
--     | xs == [True, False, False]   = [True]
--     | xs == [False, True, True]    = [True]
--     | xs == [False, True, False]   = [False]
--     | xs == [False, False, True]   = [True]
--     | xs == [False, False, False]  = [False]

pretty :: [[Bool]] -> String
pretty [] = []
-- newline rule when the end of a row is reached
pretty ([]:xss) = '\n' : pretty xss
pretty ((x : xs) : xss)
    | x          = '#' : pretty (xs : xss)
    | not x      = ' ' : pretty (xs : xss)

-- Task 04
f :: [String] 
-- f = length $ takeWhile (==1) : next f
f = "1" : next f
    where next (x:xs) = processNumber x : next xs

processNumber :: String -> String
processNumber [] = []
-- we iterate through a number, count it's consecutive occurences then move
-- on to the next digit recursively
processNumber xs@(h:_) = show (length $ takeWhile (==h) xs) ++ [h] ++ processNumber (dropWhile (==h) xs)
