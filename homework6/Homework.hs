module Homework where
--
import Data.List
-- extra import to split string
import Data.List.Split
import Data.Time.Clock ( UTCTime(..) )
import Data.Time.Calendar ( Day, gregorianMonthLength, fromGregorian )
import Data.Time.Format ( formatTime, defaultTimeLocale )
import Data.Time.Calendar.WeekDate (toWeekDate)
--

-- Task 01
data Pred =
    Val Bool | And Pred Pred | Not Pred | Or Pred Pred
    deriving (Show)

eval :: Pred -> Bool
eval (Val x) = x
eval (And x y) = eval x && eval y
eval (Or x y) = eval x || eval y
eval (Not x) = not $ eval x

-- Task 02


weekDayToNum :: String -> Int
weekDayToNum w = case w of
    "Monday" -> 1
    "Tuesday" -> 2
    "Wednesday" -> 3
    "Thursday" -> 4
    "Friday" -> 5
    "Saturday" -> 6
    "Sunday" -> 7

monthToInt :: String -> Int
monthToInt m = case m of
    "January" -> 1
    "Febrary" -> 2
    "March" -> 3
    "April" -> 4
    "May" -> 5
    "June" -> 6
    "July" -> 7
    "August" -> 8
    "September" -> 9
    "October" -> 10
    "November" -> 11
    "December" -> 12

-- brute forcing this
-- get me all the dates of a (year, month) and check 
-- what days of the week they belong to
findAllWeekdaysFromMonth :: Integer -> Int -> String -> [(Integer, Int, Int)]
findAllWeekdaysFromMonth year month weekDay = [ (year, month, d) 
                                              | d <- [1..(gregorianMonthLength year month)]
                                              , dayIsWeekDay year month d weekDay]


dayIsWeekDay :: Integer -> Int -> Int -> String -> Bool
dayIsWeekDay year month day weekDay =
  let (_, _, wday) = toWeekDate $ fromGregorian year month day
  in wday == (weekDayToNum weekDay)

ordinals = ["first", "second", "third", "last"]

getNthWeek :: String -> [(Integer, Int, Int)] -> (Integer, Int, Int)
getNthWeek n days = case n of
    "first" -> head days
    "second" -> days !! 1
    "last" -> last days
    "third" -> days !! 2
    "fourth" -> days !! 3
    "fifth" -> days !! 4


getDay :: [String] -> Integer -> Int -> Int
-- xs contains either "the first X" or "the Xteenth of"
getDay xs year month = if (last xs == "of") then dTeen else dOrd
    where 
        -- teeth case, drop last 6 chars teenth and append day 
        weekDayTeenth = (reverse (drop 6 (reverse (xs !! 1)))) ++ "day"
        weekDayOrd = xs !! 2
        -- filter out teen days
        (_, _, dTeen) =
            head $ filter (\(_, _, d) -> d `elem` [13..19]) $ findAllWeekdaysFromMonth year month weekDayTeenth
        (_, _, dOrd) = 
            getNthWeek (xs !! 1) (findAllWeekdaysFromMonth year month weekDayOrd)



dateFromDescription :: String -> Day
dateFromDescription x = fromGregorian year month day
    where 
        spl = words x
        year = read (last spl) :: Integer
        month = monthToInt $ spl !! (length spl - 2)
        day = getDay (take 3 spl) year month


-- Task 03

data Tree a
  = Leaf | Node a (Tree a) (Tree a)
  deriving (Eq, Show)
testTree = Node 1 (Node 2 Leaf Leaf) (Node 3 (Node 4 Leaf Leaf) Leaf)

-- a)
treeFilter :: (a -> Bool) -> Tree a -> Tree a
treeFilter p Leaf = Leaf
treeFilter p (Node a l r)
    | p a       = Node a (treeFilter p l) (treeFilter p r)
    | otherwise = Leaf

-- b)
levelMap :: (Int -> a -> b) -> Tree a -> Tree b
levelMap f x = applyToLevel f x 0

applyToLevel :: (Int -> a -> b) -> Tree a -> Int -> Tree b
applyToLevel f Leaf _                           = Leaf
applyToLevel f (Node x left right) currentLevel = Node (f currentLevel x) leftApply rightApply
     where leftApply  = applyToLevel f left (currentLevel + 1)
           rightApply  = applyToLevel f right (currentLevel + 1)

-- c)

-- would be more elegant if implemented with instance Eq Tree
treesIdentical :: (Eq a) => Tree a -> Tree a -> Bool
treesIdentical (Node x1 l1 r1) (Node x2 l2 r2) = (x1 == x2) && sameLeft && sameRight
    where sameLeft = (treesIdentical l1 l2)
          sameRight = (treesIdentical r1 r2)
treesIdentical Leaf Leaf = True
treesIdentical Leaf _ = False
treesIdentical _ Leaf = False

isSubtree :: Eq a => Tree a -> Tree a -> Bool
isSubtree Leaf _ = True
isSubtree subTree tree@(Node x left right) =
    treesIdentical tree subTree || isSubtree left subTree || isSubtree right subTree

-- Task 04
data Category = Null | Cat String Category deriving (Show)

parseCategories :: [String] -> [Category]
parseCategories xs = undefined

printCategories :: [Category] -> [String]
printCategories = undefined
