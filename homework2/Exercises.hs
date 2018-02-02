{-# LANGUAGE NoMonomorphismRestriction #-}
--
module Exercises where
--
import Data.List
import Data.Char
--

{-
    Here you should provide your solutions to in-class exercises.
    
    Make sure that ALL FUNCTIONS (including exXXX) have correct TYPE SIGNATURES.
    
    You should include solutions from following lectures :
    - http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-04.lhs
    - http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-05.lhs

    DON'T change function names, just remove 'undefined' and write your own
    definition for that function.
-}

{-LECTURE 04-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-04.lhs

-- EXERCISE 01 =======================================================================

-- Define 'headHunter xss' that takes the head of the first list element. If 
-- the first element has no head, it takes the head of the second element.
-- If the second element has no head, it takes the head of the third element.
-- If none of this works, the function returns an error.
ex411 = headHunter
headHunter :: [[a]] -> a
headHunter ((h : xs) : xss) = h
headHunter (a : (h : xs) : xss) = h
headHunter (a : b : (h : xs) : xss) = h
headHunter _ = error "none"

-- Define 'firstColumn m' that returns the first column of a matrix.
-- firstColumn [[1,2],[3,4]] => [1,3]
-- Check what happens if the input is not a valid matrix.
ex412 = firstColumn
firstColumn :: [[a]] -> [a]
firstColumn m = [h | (h:_) <- m]

-- Define 'shoutOutLoud' that repeats three times the initial letter of each
-- word in a string.
-- shoutOutLoud :: String -> String
-- shoutOutLoud "Is anybody here?" => "IIIs aaanybody hhhere?"
ex413 = shoutOutLoud
shoutOutLoud :: String -> String
shoutOutLoud xs = unwords [h : h : h : xs | (h:xs) <- words xs]

-- EXERCISE 02 =======================================================================

-- Define 'pad' that pads the shorter of two the strings with trailing spaces 
-- and returns both strings capitalized.
-- pad :: String -> String -> (String, String)
-- pad "elephant" "cat" => ("Elephant", "Cat     ")
ex421 = pad
pad :: String -> String -> (String, String)
pad xs@(hx:x) ys@(hy:y)
    | lxs > lys = (toUpper hx : x, toUpper hy : y ++ padding)
    | otherwise = (xs ++ padding, ys)
    where lxs = length xs
          lys = length ys
          longerOne = max lxs lys
          shorterOne = min lxs lys
          padding = replicate (longerOne - shorterOne) ' '

-- Define 'quartiles xs' that returns the quartiles (q1,q2,q3) of a given list.
-- The quartiles are elements at the first, second, and third quarter of a list
-- sorted in ascending order. (You can use the built-int 'splitAt' function and
-- the previously defined 'median' function.)
-- quartiles :: [Int] -> (Double,Double,Double)
-- quartiles [3,1,2,4,5,6,8,0,7] => (1.5, 4.0, 6.5)
median :: (Integral a, Fractional b) => [a] -> b
median [] = error "median: Empty list"
median xs 
  | odd l     = realToFrac $ ys !! h
  | otherwise = realToFrac (ys !! h + ys !! (h-1)) / 2
  where l  = length xs
        h  = l `div` 2
        ys = sort xs

ex422 = quartiles
quartiles :: [Int] -> (Double, Double, Double)
quartiles xs = (q1, q2, q3)
    where xs_sort = sort xs
          xs_len = length xs
          q2 = median xs_sort
          q1 = median $ fst $ splitAt (div xs_len 2) xs_sort
          q3 = median $ snd $ splitAt (div xs_len 2 + 1) xs_sort

-- EXERCISE 03 =======================================================================

-- Redo Exercise 2 using 'let' instead of 'where'.
ex431 = pad'
pad' :: String -> String -> (String, String)
pad' xs@(hx:x) ys@(hy:y) = 
    let lxs = length xs
        lys = length ys
        longerOne = max lxs lys
        shorterOne = min lxs lys
        padding = replicate (longerOne - shorterOne) ' '
        in case lxs > lys of 
            True -> (toUpper hx : x, toUpper hy : y ++ padding)
            False -> (xs ++ padding, ys)

ex432 = quartiles'
quartiles' :: [Int] -> (Double, Double, Double)
quartiles' xs =       
        let xs_sort = sort xs
            xs_len = length xs
            q2 = median xs_sort
            q1 = median $ fst $ splitAt (div xs_len 2) xs_sort
            q3 = median $ snd $ splitAt (div xs_len 2 + 1) xs_sort
        in (q1, q2, q3)


-- EXERCISE 04 =======================================================================

-- Write a function that takes in a pair (a,b) and a list [c] and returns the
-- following string:
-- "The pair [contains two ones|contains one one|does not contain a single one]
-- and the second element of the list is <x>"
ex441 :: (Eq a, Num a, Show c) => (a, a) -> [c] -> String
ex441 (1, 1) (_ : h2 : _) = "The pair contains two ones and the second element of the list is " ++ show h2
ex441 (_, 1) (_ : h2 : _) = "The pair contains one one and the second element of the list is " ++ show h2
ex441 (1, _) (_ : h2 : _) = "The pair contains one one and the second element of the list is " ++ show h2
ex441 (_, _) (_ : h2 : _) = "The pair does not contain a single one and the second element of the list is " ++ show h2

{-LECTURE 05-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-05.lhs

-- EXERCISE 01 =======================================================================

-- Define a recursive function to compute the product of a list of elements.
-- product' :: Num a => [a] -> a
ex511 = product'
product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

-- Define a recursive function 'headsOf' that takes a list of lists and
-- returns a list of their heads.
-- headsOf :: [[a]] -> [a]
-- headsOf [[1,2,3],[4,5],[6]] => [1,4,6]
ex512 = headsOf
headsOf :: [[a]] -> [a]
headsOf [] = []
headsOf ([]:xss) = headsOf xss
headsOf ((x:_):xss) = x : headsOf xss

-- EXERCISE 02 =======================================================================

-- Define a recursive function 'modMult n m xs' that multiplies each element of
-- a list 'xs' with 'n' modulo 'm'.
modMult :: (Integral a) => a -> a -> [a] -> [a]
ex521 = modMult
modMult _ _ [] = []
modMult n m (x:xs) = (n `mod` m *  x) : modMult n m xs

-- Define a function 'addPredecessor' that adds to each element of a list the
-- value of the preceding element. The first element gets no value added.
-- addPredecessor :: Num a => [a] -> [a]
-- addPredecessor [3,2,1] => [3,5,3]
ex522 = addPredecessor
addPredecessor :: Num a => [a] -> [a]
addPredecessor xs = add 0 xs
    where   add n (x:xs) = n + x : add x xs
            add _ [] = []

-- EXERCISE 03 =======================================================================

-- Define 'equalTriplets' that filters from a list of triplets (x,y,z) all
-- triplets for which x==y==z.
-- equalTriplets [(1,2,3),(2,2,2),(4,5,6)] => [(2,2,2)]
ex531 = equalTriplets
equalTriplets :: Eq a => [(a, a, a)] -> [(a, a, a)]
equalTriplets [] = []
equalTriplets ((a, b, c):xs) | a == b && b == c = (a, b, c) : equalTriplets xs
                             | otherwise = equalTriplets xs

-- Define your own version of the replicate function:
-- replicate' :: Int -> a -> [a]
ex532 = replicate'
replicate' :: Integral a => a -> b -> [b]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

-- EXERCISE 04 =======================================================================

-- Define your own recursive version of the drop function:
-- drop' :: Int -> [a] -> [a].
-- Define drop'' (a wrapper function) so that for n < 0 the function drops
-- the elements from the end of the list. You can use 'reverse'.
ex541 = drop'
drop' :: Int -> [b] -> [b]
drop' 0 xs = xs
drop' n (x:xs) = drop (n-1) xs


ex541' = drop''
drop'' :: Int -> [b] -> [b]
drop'' n xs = reverse (drop' n (reverse xs))

-- Define a recursive function 'takeFromTo n1 n2 xs'.
-- takeFromTo :: Int -> Int -> [a] -> [a]
ex542 = takeFromTo
takeFromTo :: Int -> Int -> [a] -> [a]
takeFromTo n1 n2 (x:xs) | n1 < n2 = x : takeFromTo (n1 + 1) n2 xs
                        | otherwise = []

-- EXERCISE 05 =======================================================================

-- Define a recursive function 'eachThird' that retains every third element
-- in a list.
-- eachThird :: [a] -> [a]
-- eachThird "zagreb" => "gb"
ex551 = eachThird
eachThird :: [a] -> [a]
eachThird (x:y:z:xs) = z : eachThird xs
eachThird _ = []

-- Define a recursive function 'crossZip' that zips two lists in a "crossing"
-- manner:
-- crossZip [1,2,3,4,5] [4,5,6,7,8] => [(1,5),(2,4),(3,7),(4,6)]
ex552 = crossZip
crossZip :: [a] -> [b] -> [(a,b)]
crossZip (x1:x2:xs) (y1:y2:ys) = (x1, y2) : (x2, y1) : crossZip xs ys
crossZip [x] _ = []
crossZip _ [x] = []
crossZip [] _ = []
crossZip _ [] = []

-- EXERCISE 06 =======================================================================

-- Write an accumulator-style recursive definition of
-- length' :: [a] -> Int

ex561 = length'
length' = undefined

-- Write an accumulator-style recursive definition of
--     maxUnzip :: [(Int, Int)] -> (Int, Int)
-- that returns the maximum element at the first position and the maximum
-- element at the second position in a pair, i.e., it's equivalent to:
--     maxUnzip zs = (maximum xs, maximum ys)
--         where (xs,ys) = unzip zs
-- If the list is empty, return an "empty list" error.
-- Now write a standard recursive definition maxUnzip' (without an accumulator).
ex562 = maxUnzip
maxUnzip = undefined

ex562' = maxUnzip'
maxUnzip' = undefined
