module Homework where
--
import Data.List
import Data.Char
import Data.Function ( fix )
--

-- Task 01

-- non accumulator style
factorial :: (Num a, Eq a) => a -> a
factorial = fix (\rec x -> if x == 0 then 1 else x * rec(x - 1))

-- non accumulator style
sum' :: Num a => [a] -> a
sum' = fix (\rec (x:xs) -> if null xs then x else x + rec xs)

-- accumulator style
factorial' :: (Num a, Eq a) => a -> a
factorial' n = fix (\rec x -> if x == n then x else x * rec (x + 1)) 1

-- accumulator style
sum'' :: Num a => [a] -> a
sum'' xs = fix (\rec (x:xs) s -> if null xs then x + s else rec xs (x + s)) xs 0

nats :: [Integer]
nats = fix (\rec x -> x : rec (x + 1)) 0

map' :: (a -> b) -> [a] -> [b]
map' f = fix (\rec (x:xs) -> if null xs then [f x] else f x : rec xs)

zip' :: [a] -> [b] -> [(a, b)]
zip' = fix (\rec (x:xs) (y:ys) -> if null xs || null ys then [(x, y)] else (x, y) : rec xs ys)

-- Task 02
subsets :: (Eq a) => Int -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
-- recursively call to prepend the head of the list to each element inside
-- for example 2 [1, 2, 3]
-- map (1:) ((map (2:) [[]] ++ map (3:) [[]])) ++ map(2:) (map(3:) [[]])
subsets k (x:xs) = map (x:) (subsets (k - 1) distinctList) ++ subsets k distinctList
    where distinctList = nub xs

partitions :: [a] -> [[[a]]]
partitions [] = error "invalid input"
partitions xs = partition' xs
partition' :: [a] -> [[[a]]]
partition' [] = [[]]
partition' (x:xs) = map ([x]:) (partition' xs) ++ [(x:ys):yss | (ys:yss) <- partition' xs]

-- Task 03
permutations' :: [a] -> [[a]]
permutations' = undefined
