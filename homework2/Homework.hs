module Homework where
--
import Data.List
import Data.Char
--

-- Task 01
toRNA :: String -> String
toRNA [] = []
toRNA (x : xs)
    | x == 'G' = 'C' : toRNA xs
    | x == 'C' = 'G' : toRNA xs
    | x == 'T' = 'A' : toRNA xs
    | x == 'A' = 'U' : toRNA xs
    | otherwise = error "Nucleotide doesn't exist"

-- Task 02
multiply :: Int -> Int -> Int
multiply 0 _ = 0
multiply a b = b + multiply (a-1) b

divide :: Int -> Int -> Int
divide _ 0 = error "Dividing by zero is infinity"
divide a b
        | a - b >= 0 = 1 + divide (a-b) b
        | otherwise = 0

greatestCD :: Int -> Int -> Int
greatestCD a 0 = a
greatestCD a b 
    | a < b     = gcd b a `mod` b
    | a == b    = a
    | otherwise = gcd (b `mod` a) a

-- Task 03
ones = ["", "one","two","three","four","five","six","seven","eight","nine"]
teens = ["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]
tens = ["", "", "twenty", "thirty", "fourty", "fifty", "sixty", "seventy", "eighty", "ninety"] 

doOnes :: Int -> String
doOnes x = ones !! x
doTeens :: Int -> String
doTeens x = teens !! (x - 10) 
doTens :: Int -> String
doTens x = tens !! (x `div` 10)
-- this is imperfect, as it does not traverse all digits (which is most
-- likely the desired solution so the string outputted won't fully align
-- for 0 digit places)
-- numbers such as 100324 will not work since there is no print out of
-- thousand in that case
-- also does not work for negative numbers and zero
-- it does have an advantage to do less invocations for 0 digit inputs
numberToWords :: Int -> String
numberToWords x = cleanUpNumber $ transformNumberToWord x

cleanUpNumber :: String -> String
cleanUpNumber xs = unwords [removeTrailingDash w | w <- words xs]

removeTrailingDash :: String -> String
removeTrailingDash x = if last x == '-' then [c | c <- x, c/='-'] else x

transformNumberToWord :: Int -> String
transformNumberToWord x
    | x < 10 = doOnes x
    | x < 20 = doTeens x
    | x < 100 = doTens x ++ "-" ++ numberToWords (x `mod` 10)
    | x < 1000 = doOnes (x `div` 100) ++ " hundred " ++ numberToWords (x `mod` 100)
    | x < 10000 = doOnes (x `div` 1000) ++ " thousand " ++ numberToWords (x `mod` 1000)
    | x < 20000 = doTeens (x `div` 1000) ++ " thousand " ++ numberToWords (x `mod` 1000)
    | x < 100000 = numberToWords (x `div` 1000) ++ " hundred thousand " ++ numberToWords (x `mod` 1000)
    | x < 1000000 = numberToWords (x `div` 100000) ++ " hundred " ++ numberToWords (x `mod` 100000)
    | x < 10000000 = numberToWords (x `div` 1000000) ++ " million " ++ numberToWords (x `mod` 1000000)

-- Task 04
undefined' :: a
undefined' = error "Prelude.undefined"
-- undefined is something that always throws an exception, yet it compiles
-- (matches everything)
-- for this reason we can simply implement it as the output of an error
-- (should match everything) and copy the message from undefined as an argument to the
-- error function
-- exception message which it gives
-- (a, b) = (2, undefined')
-- a is 2
