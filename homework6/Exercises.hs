{-# LANGUAGE NoMonomorphismRestriction #-}
--
module Exercises where

import System.Exit
import Data.Set as Set
import Data.List as List
import Text.Read
import Data.Maybe
import Data.Char
import System.IO
import System.Environment
import System.FilePath
import System.Random
import System.FilePath
import System.Directory
import Control.DeepSeq
import Control.Monad
--

{-
    Here you should provide your solutions to in-class exercises.

    Make sure that ALL FUNCTIONS (including exXXX) have correct TYPE SIGNATURES.

    You should include solutions from following lectures :
    http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-10.lhs
    http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-11.lhs

    DON'T change function names, just remove 'undefined' and write your own
    definition for that function.
-}

{-LECTURE 10-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-10.lhs

-- EXERCISE 01 =======================================================================
{-
  1.2.
  - Define a function
    parentCheck :: Person2 -> Bool
    that checks whether the given person is one of the children of its parents.
-}

data Sex = Male | Female deriving (Show,Read,Ord,Eq)

data Person2 = Person2 {
  personId2 :: String,
  forename2 :: String,
  surname2  :: String,
  sex2      :: Sex,   --- data Sex = Male | Female deriving (Show,Read,Eq,Ord)
  mother2   :: Maybe Person2,
  father2   :: Maybe Person2,
  partner2  :: Maybe Person2,
  children2 :: [Person2] } deriving (Show,Read,Ord)

instance Eq Person2 where
  p1 == p2 = personId2 p1 == personId2 p2

getKids :: Maybe Person2 -> [Person2]
getKids (Just p) = children2 p
getKids Nothing  = []

parentCheck :: Person2 -> Bool
parentCheck p = p `elem` mothersChildren && p `elem` fathersChildren
   where mothersChildren = getKids (mother2 p)
         fathersChildren = getKids (father2 p)

john = Person2 "123" "John" "Doe" Male Nothing Nothing Nothing [bohn]
daddy = Person2 "124" "Daddy" "Daddic" Male Nothing Nothing Nothing [bohn] 
bohn = Person2 "125" "Bohn" "Boe" Male (Just john) (Just daddy) Nothing []
nonbohn = Person2 "126" "Non-Bohn" "Non-Boe" Male (Just john) (Just daddy) Nothing []

{-
  1.3.
  - Define a function
    sister :: Person2 -> Maybe Person2
    that returns the sister of a person, if such exists.
-}

intersectLists :: (Eq a) => [a] -> [a] -> [a]
intersectLists l1 l2 = [x  | x <- l1, y <- l2, x == y]

mom = Person2 "127" "John" "Doe" Male Nothing Nothing Nothing [son, daughter]
dad = Person2 "128" "Daddy" "Daddic" Male Nothing Nothing Nothing [son, daughter] 
son = Person2 "129" "Bohn" "Boe" Male (Just mom) (Just dad) Nothing []
daughter = Person2 "130" "Non-Bohn" "Non-Boe" Male (Just mom) (Just dad) Nothing []

-- (sister son) == (Just daughter)

sister :: Person2 -> Maybe Person2
sister p
 | length otherKids > 0 = Just (head otherKids)
 | otherwise = Nothing
   where mothersChildren = getKids (mother2 p)
         fathersChildren = getKids (father2 p)
         otherKids = List.delete p (intersectLists mothersChildren fathersChildren)

{-
  1.4.
  - Define a function that returns all descendants of a person.
    descendant :: Person2 -> [Person2]
-}
grandpa = Person2 "131" "Grandad" "Oldman" Male Nothing Nothing Nothing [dad]
gDad = Person2 "132" "Daddy" "Daddic" Male Nothing Nothing Nothing [son] 
gSon = Person2 "133" "Bohn" "Boe" Male Nothing Nothing Nothing []

descendant :: Person2 -> [Person2]
descendant p =  children2 p ++ kids (children2 p)

kids :: [Person2] -> [Person2]
kids [] = []
kids (x:xs) = x : kids xs

-- EXERCISE 02 =======================================================================
{-
  2.1.
  - Define
    listHead :: MyList a -> Maybe a
-}

data MyList a = Empty | Cons a (MyList a) deriving (Show,Read,Eq,Ord)

listHead :: MyList a -> Maybe a
listHead Empty = Nothing
listHead (x `Cons` _) = Just x

{-
  2.2.
  - Define a function that works like 'map' but works on a 'MyList' type:
    listMap :: (a -> b) -> MyList a -> MyList b
-}

listMap :: (a -> b) -> MyList a -> MyList b
listMap f Empty = Empty
listMap f (x `Cons` y) = (f x) `Cons` (listMap f y)

-- EXERCISE 03 =======================================================================
{-
  3.1.
  - Define a function
    treeMax :: Ord a => Tree a -> a
    that finds the maximum element in a tree. Return an error if the tree is
    empty.
-}

data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show,Eq)

treeMax :: Ord a => Tree a -> a
treeMax Null = error "empty tree"
treeMax (Node x _ Null) = x
treeMax (Node x left right) = treeMax right

{-
  3.2.
  - Define a function
    treeToList :: Ord a => Tree a -> [a]
    that will collect in a list all elements from inner nodes of a tree by doing
    an in-order (left-root-right) traversal.
-}

treeToList :: Ord a => Tree a -> [a]
treeToList (Node x Null Null) = [x]
treeToList Null = []
treeToList (Node x left right) = treeToList left ++ [x] ++ treeToList right

{-
  3.3.
  - Define a function to prune the tree at a given level (root has level 0).
    levelCut :: Int -> Tree a -> Tree a
-}

prune :: Tree a -> Int -> Int -> Tree a
prune Null _ _ = Null
prune (Node x left right) pruneLevel currentLevel
  | pruneLevel == currentLevel = Node x Null Null
  | otherwise = Node x (prune left pruneLevel (currentLevel + 1)) (prune right pruneLevel (currentLevel + 1))

levelCut :: Int -> Tree a -> Tree a
levelCut pruneLevel tree = prune tree pruneLevel 0

-- EXERCISE 04 =======================================================================
{-
  4.1.
  - Define a function that converts a list into a sorted tree.
    listToTree :: Ord a => [a] -> Tree a
-}

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x Null = Node x Null Null
treeInsert x t@(Node y l r) 
  | x < y     = Node y (treeInsert x l) r
  | x > y     = Node y l (treeInsert x r)
  | otherwise = t

listToTree :: Ord a => [a] -> Tree a
listToTree = List.foldr treeInsert Null

{-
  4.2.
  - Using 'listToTree' and 'treeToList' defined previously, define these two 
    functions, define:
    sortAndNub :: Ord a => [a] -> [a]
-}

sortAndNub :: Ord a => [a] -> [a]
sortAndNub = treeToList . listToTree

-- EXERCISE 05 =======================================================================
{-
  5.1.
  - Define an 'Eq' instance for the 'Weekday' type that works like (==), except
    that two Fridays are never identical.
-}

data Weekday = 
  Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show,Enum)

{-
  5.2.
  - Define 'Person' as an instance of 'Show' type class so that instead of the
    values of partners and children only the respective person names are shown,
    which will enable the print out of an infinite structure of this type.
-}

data Person = Person
  { idNumber :: String
  , forename :: String
  , surname  :: String
  , sex      :: Sex
  , age      :: Int
  , partner  :: Maybe Person
  , children :: [Person]
  } deriving (Show,Read,Ord,Eq)


{-LECTURE 11-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-11.lhs

-- EXERCISE 01 =======================================================================

{- DON'T FORGET TO WRITE TYPE SIGNATURES-}

{-
  1.1.
  - Define a 'main' function that reads in two strings and prints them out
    concatenated and reversed.
-}

main :: IO ()
main = do
    putStrLn "Enter two strings"
    s1  <- getLine
    s2 <- getLine
    putStrLn $ reverse s1 ++ reverse s2

{-
  1.2.
  - Write a function 'threeNumbers' that reads in three numbers and prints out
    their sum.
-}

threeNumbers :: IO ()
threeNumbers = do
    putStrLn "Enter three numbers"
    n1 <- getLine
    n2 <- getLine
    n3 <- getLine
    let sum = (read n1 :: Int) + (read n2 :: Int) + (read n3 :: Int)
    print sum

-- EXERCISE 02 =======================================================================
{-
  2.1.
  - Define a function 'threeStrings' that reads in three strings and outputs them
    to the screen as one string, while it returns its total length.
    treeStrings :: IO Int
-}

threeStrings :: IO Int
threeStrings = do
    putStrLn "Enter three strings"
    s1 <- getLine
    s2 <- getLine
    s3 <- getLine
    let total = s1 ++ s2 ++ s3
    putStrLn total
    return (length total)

{-
  2.2.
  - Define a function 'askNumber9' that reads in a number and returns that number
    converted into an 'Int'. Input should be repeated until the user enters a
    number (a string containing only digits).
      askNumber9 :: IO Int
-}

askNumber9 :: IO Int
askNumber9 = do
    putStrLn "Enter a valid digit"
    number <- getLine
    let numInt = readMaybe number :: Maybe Int
    if numInt == Nothing then do
      putStr "Invalid input! "
      askNumber9
    else return (fromJust numInt)

{-
  2.3.
  - Define a function 'askUser m p' that returns an action that prints out 'm',
    reads in a string from the input, repeats the input until the input
    string satisfies the function 'p', and then returns the input string.
      askUser :: String -> (String -> Bool) -> IO String
  - Generalize this function to
      askUser' :: Read a => String -> (String -> Bool) -> IO a
-}

askUser :: String -> (String -> Bool) -> IO String
askUser m p = do
    return (putStrLn "m")
    str <- getLine
    if p str /= True then do
        askUser str p
    else return str

-- I don't get what I'm suppoused to return in askUser'
askUser' :: Read a => String -> (String -> Bool) -> IO a
askUser' m p = undefined

{-
  2.4.
  - Define a function that reads in strings until the user inputs an empty
    string, and then returns a list of strings received as input.
      inputStrings :: IO [String]
-}

inputStrings :: IO [String]
inputStrings = infGo []

infGo :: [String] -> IO [String]
infGo strings = do
    line <- getLine
    if line == ""
        then return strings
        else infGo (line : strings)

-- EXERCISE 03 =======================================================================
{-
  3.1.
  - Define a function that reads in a number, then reads in that many
    strings, and finally prints these strings in reverse order.
-}

{-
  3.2.
  - Give recursive definitions for 'sequence' and 'sequence_'.
-}

{-
  3.3.
  - Give a recursive definitions for 'mapM' and 'mapM_'.
-}

{-
  3.4.
  - Define a function that prints out the Pythagorean triplets whose all sides
    are <=100. Every triplet should be in a separate line.
-}

-- EXERCISE 04 =======================================================================
{-
  4.1.
  - Define a function that removes from standard input every second line and
    prints the result to standard output.
      filterOdd :: IO ()
-}

giveIndex :: (Enum a, Num a) => [b] -> [(a, b)]
giveIndex = zip [0..]

filterOdd :: IO ()
filterOdd = interact (unlines . List.map (snd) . List.filter (odd . fst) . giveIndex . lines )


{-
  4.2.
  - Define a function that prefixes each line from standard input with a line
    number (number + space).
      numberLines :: IO ()
-}
addLineNum :: (Int, String) -> String
addLineNum (num, str) = show num ++ " " ++ str

numberLines :: IO ()
numberLines = interact (unlines . List.map (addLineNum) . giveIndex . lines )




{- 4.3.
  - Define a function to remove from standard input all words from a given set of
    words.
      filterWords :: Set String -> IO ()
-}

filterWords :: Set String -> IO ()
filterWords ws = interact (unwords . List.filter(flip (notElem) ws) . words) 

-- EXERCISE 05 =======================================================================
{-
  5.1.
  - Define a function
    wc :: FilePath -> IO (Int, Int, Int)
    that counts the number of characters, words, and lines in a file.
-}

-- wc :: FilePath -> IO (Int, Int, Int)

wc :: FilePath -> IO (Int, Int, Int)
wc f = withFile f ReadMode $ \h -> do
    str <- hGetContents h
    let chrLen = length str
    let wordNum = length $ words str
    let lineNum = length $ lines str
    str `deepseq` return (chrLen, wordNum, lineNum)

{-
  5.2. 
  - Define a function
    copyLines :: [Int] -> FilePath -> FilePath -> IO ()
    that copies given lines from the first file into the second.
-}

copyLines :: [Int] -> FilePath -> FilePath -> IO ()
copyLines linesToFilter src dst = do
    h <- openFile src ReadMode
    h2 <- openFile dst WriteMode
    s <- hGetContents h
    -- I need to flip the order since the filter function is partial 
    let filtered = unlines $ List.map (snd) $ List.filter ((flip elem) linesToFilter . fst) (zip [0..] $ lines s)
    hPutStr h2 filtered
    hClose h2
    hClose h

-- EXERCISE 06 =======================================================================
{-
  6.1.
  - Define a function
      wordTypes :: FilePath -> IO Int
    to compute the number of distinct words in the given file.
-}

wordTypes :: FilePath -> IO Int
wordTypes f = do
    s <- readFile f
    return (length $ nub $ words s)


{-
  6.2.
  - Define a function 
      diff :: FilePath -> FilePath -> IO ()
    that takes two file names, compares their corresponding lines, and then
    outputs to standard output all lines in which the files differ. Lines should 
    be printed one below the other, prefixed with "<" for the first and ">" for
    the second file.
-}
linesNotIn :: [String] -> [String] -> [String]
linesNotIn a b = [x | x <- a,  x `notElem` b]

diff :: FilePath -> FilePath -> IO ()
diff f1 f2 = do
    h1 <- openFile f1 ReadMode
    h2 <- openFile f2 ReadMode
    s1 <- hGetContents h1
    s2 <- hGetContents h2
    let aNotInB = unlines $ List.map (\x -> "< " ++ x) $ linesNotIn (lines s1) (lines s2)
    let bNotInA = unlines $ List.map (\x -> "> " ++ x) $ linesNotIn (lines s2) (lines s1)
    putStr aNotInB
    putStr bNotInA
    hClose h1
    hClose h2

{-
  6.3.
  - Define a function
      removeSpaces :: FilePath -> IO () 
    that removes trailing spaces from all lines in the given file.
    The function should change the original file.
-}

removeTrailSpace :: String -> String
removeTrailSpace s = reverse $ dropWhile (==' ') (reverse s)

removeSpaces :: FilePath -> IO () 
removeSpaces file = do
    contents <- readFile file
    putStrLn contents
    let newContent = unlines $ List.map (removeTrailSpace) (lines contents)
    writeFile file newContent

-- EXERCISE 07 =======================================================================
{-
  7.1.
  - Define a function
      fileHead :: IO ()
    that prints the first 'n' lines from a file. The name of the file and the
    number of lines are specified at the command line, e.g.:
      filehead -5 input.txt
    If the number of lines is missing, default to 10. If file name is missing,
    read from the standard input. If the file doesn't exist, print an error
    message and exit with failure using 'exitFailure' from 'System.Exit'.
-}

-- unlines $ take n (lines s)

getFileOrThrow :: FilePath -> IO Handle
getFileOrThrow f = do 
    e <- doesFileExist f
    if e then openFile f ReadMode else
        do putStrLn "file doesn't exist" 
           exitFailure
 

fileHead :: IO ()
fileHead = do
    xs <- getArgs
    -- first we're gonna process the file stream
    h <- case xs of
      (_:f:_) -> do getFileOrThrow f
      (f:_)   -> getFileOrThrow f
      []      -> return stdin
    s <- hGetContents h

    -- now we get the number of lines to show
    let numLinesToShow = case xs of (n:_:_) -> read n :: Int
                                    _   -> 10

    putStr $ unlines $ take numLinesToShow (lines s)


{-
  7.2.
  - Define a function
      sortFiles :: IO ()
    that sorts lines from multiple files and prints them to standard output.
    File names are provided at the command line.
    "sortFiles file1.txt file2.txt file3.txt"
    If any of the files does not exist, print an error message.
-}

sortFiles :: IO ()
sortFiles = do
    xs <- getArgs

    ys <- forM xs $ \x -> do
        h <- getFileOrThrow x
        s <- hGetContents h
        return $ lines s

    putStrLn $ unlines $ sort $ concat ys


-- EXERCISE 08 =======================================================================
{-
  8.1.
  - Define your own implementation of
      randoms' :: (RandomGen g, Random a) => g -> [a]
-}

randoms' :: (Random a, RandomGen g) => g -> [a]
randoms' gen = (fst (random gen)) : (randoms' gen)


{-
  8.2.
  - Define a function
      randomPositions :: Int -> Int -> Int -> Int -> IO [(Int,Int)]
    that returns a list of randomly generated integer coordinates from within a
    given interval.
      randomPositions 0 10 0 10 => [(2,1),(4,3),(7,7),...
-}

randomPositions :: Int -> Int -> Int -> Int -> IO [(Int,Int)]
randomPositions x1 x2 y1 y2 = do
    let intervalX = x2 - x1 + 1
    let intervalY = y2 - y1 + 1
    let xs = randomRs (x1, x2) (mkStdGen 1) :: [Int]
    let ys = randomRs (y1, y2) (mkStdGen 2):: [Int]
    return $ zip xs ys
