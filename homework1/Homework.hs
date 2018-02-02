import Data.Char
import Data.List hiding (insert, lookup)
import Prelude hiding (insert, lookup)
-- Tasks

-- ==========================================================
-- 1. (2 points)
-- ==========================================================
isLeapYear :: Int -> Bool

isLeapYear x
    | x `mod` 400 == 0 = True
    | x `mod` 100 == 0 = False
    | x `mod` 4 == 0 = True
    | otherwise = False

leapList = [ x | x <- [1996..2017],  isLeapYear x]

-- ==========================================================
-- 2. (3 points)
-- ==========================================================
evaluate :: Double -> [Double] -> Double

-- assuming the coefficient ordering is from 0..n as displayed in the
-- equation
-- power operator precedence over multiplication makes sure the correct
-- order of execution
evaluate x a = sum [ x ^ index * coef  | (index, coef) <- zip [0..]  a ]

factorial :: Double -> Double

-- first defining factorial(0) = 1 so we have the recursive end condition
factorial 0 = 1
-- we define the formula of the factorial
factorial n = n * factorial (n - 1)

-- Maclaurin series
-- function has prededence over division
maclaurin = [ 1 / factorial x | x <- [0..] ]

-- exp function for first 170 elements, we make the maclaurin series 
-- the coefficient for the polynomial series sum
exp' x = evaluate x $ take 170 maclaurin 

-- ==========================================================
-- 3. (3 points)
-- ==========================================================

-- I'm first getting all elements that match the key, then taking the first
-- to ensure only one element is returned
-- (a)
findItem :: [(String, a)] -> String -> [(String, a)]
findItem myMap keyToSearch 
    | not $ null valueRetrieved = [ head valueRetrieved]                         
    | otherwise = []
    where valueRetrieved = [(key, value) | (key, value) <- myMap, key == keyToSearch]

-- (b)
contains :: [(String, a)] -> String -> Bool
contains myMap keyToSearch = not $ null [ key | (key, value) <- myMap, key == keyToSearch]

-- (c)
lookup :: [(String, a)] -> String -> a
lookup myMap keyToSearch
    | not $ null valueSearched = snd $ head valueSearched
    | otherwise = error "Key not found."
    where valueSearched = findItem myMap keyToSearch

-- (d)
-- if doesn't contain, add it, 
-- otherwise return what you get at input
insert :: [(String, a)] -> (String, a) -> [(String, a)]
insert myMap pairToInsert
    | not $ contains myMap $ fst pairToInsert = pairToInsert : myMap
    | otherwise = myMap

-- (e)
remove :: [(String, a)] -> String -> [(String, a)]
remove myMap keyToSearch = [(key, value) | (key, value) <- myMap, key /= keyToSearch]

-- (f)
-- search if value is contained in list
-- if not, return list as is
-- otherwise remove the key-value pair and insert the new ones
update :: [(String, a)] -> String -> a -> [(String, a)]
update myMap keyToUpdate valueForNewKey
    | not $ contains myMap keyToUpdate = myMap
    | otherwise = insert ( remove myMap keyToUpdate ) (keyToUpdate, valueForNewKey)

-- ==========================================================
-- 4. (3 points)
-- ==========================================================
-- cosineSimilarity :: String -> String -> Double
cleanWord :: String -> String
cleanWord w = [toLower c | c <- w, isLetter c]

-- counts occurences of word wordToCount in string xss
countWords :: Num a => [String] -> String -> a
countWords xss wordToCount = sum [1 | xs <- xss, xs == wordToCount]

-- count how many times each distinct word occurs in a string
-- first gets all distinct words then for each counts its occurences
wordFrequencyVector :: Num a => [String] -> [(String, a)]
wordFrequencyVector xs = [ (word, countWords xs word) | word <- nub xs]

-- removes non alphanumeric chars
preprocessSentence :: String -> [String]
preprocessSentence xs = [cleanWord w | w <- words xs]

-- gets the intersection of words from two word-frequency vectors, see
-- wordFrequency vector how those are calculated
commonWordsFromFreqVector :: Num a => [(String, a)] -> [(String, a)] -> [String]
commonWordsFromFreqVector xs1 xs2 = nub [w1 | (w1, _) <- xs1, (w2, _ )  <- xs2, w1 == w2]

-- getting common words and multiplying their frequencies
numerator :: Num a => [(String, a)] -> [(String, a)] -> a
numerator v1 v2 = sum [ a * b | w <- commonWordsFromFreqVector v1 v2, let a = lookup v1 w, let b = lookup v2 w]

-- vector magnitude 
absolute :: Num a => [(String, a)] -> a
absolute v = sum [ count * count | (word, count) <- v]

-- using Floating in signature so that division is floating point
cosineSimilarity :: (Floating a) => String -> String -> a
cosineSimilarity s1 s2 = numerator v1 v2 / denominator
    where   v1 = wordFrequencyVector $ preprocessSentence s1
            v2 = wordFrequencyVector $ preprocessSentence s2
            aSquared = sqrt $ absolute v1
            bSquared = sqrt $ absolute v2
            denominator = aSquared * bSquared
