module Tree where

import Data.Char
import Data.List
import Data.Ord

containsNumber :: String -> Bool
containsNumber = any (`elem` ['0'..'9'])

sameLetterTwice :: String -> Bool
sameLetterTwice xs = any ((>) 1 . snd) (map (\x -> (head x, length x)) $ group $ sort xs)

isWeird :: String -> Bool
isWeird w = containsNumber w || sameLetterTwice w

weirdFilter :: [String] -> [String]
weirdFilter = filter isWeird


sortGuys :: (String, Int) -> (String, Int) -> Ordering
sortGuys (a1, b1) (a2, b2)
  | b1 < b2 = GT
  | b1 > b2 = LT
  | b1 == b2 = compare a1 a2

ranking :: [(String, Int)] -> [String]
ranking xs = map fst (sortBy sortGuys xs)


shorten n (x:xs)
  | length x < n  = []
  | otherwise     = x : shorten n xs

zipN :: [[a]] -> [[a]]
zipN xs =  shorten n (transpose xs)
    where n = length xs


mapWhile :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapWhile cond apply [] = []
mapWhile cond apply all@(x:xs) = singleElApply x x : mapWhile cond apply xs
  where singleElApply a acc = if cond acc then singleElApply a (apply acc) else acc


capWord :: String -> [String] -> String
capWord xs@(h:x) forb = if xs `notElem` forb then toUpper h : x else xs

capitalise :: String -> [String] -> String
capitalise xs xss = unwords [capWord w xss | w <- words xs]
