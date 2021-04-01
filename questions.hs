-- Problem 1
-- Find the last element of a list.
import Data.List

myLast :: [a] -> a
myLast = last

myLast' :: [a] -> a
myLast' [x] = x
myLast' (_ : xs) = myLast' xs

-- Problem 2
-- Find the last but one element of a list.
myButLast :: [a] -> a
myButLast xs = reverse xs !! 1

-- Problem 3
-- Find the K'th element of a list. The first element in the list is number 1.
myElementAt :: [a] -> Int -> a
myElementAt xs n = last $ take n xs

-- Problem 4
-- Find the number of elements of a list.
myLength :: [a] -> Int
myLength = foldl (\x _ -> x + 1) 0

-- Problem 5
-- Reverse a list.
myReverse :: [a] -> [a]
myReverse [x] = [x]
myReverse (x : xs) = myReverse xs ++ [x]

-- Problem 6
-- Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
myIsPalindrome :: Eq a => [a] -> Bool
myIsPalindrome xs = reverse xs == xs

-- Problem 7
-- Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List x) = concatMap myFlatten x

-- Problem 8
-- Eliminate consecutive duplicates of list elements.
myCompress :: Eq a => [a] -> [a]
myCompress = map head . group

-- Problem 9
-- Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
myPack :: Eq a => [a] -> [[a]]
myPack [] = []
myPack (x : xs) = (x : takeWhile (== x) xs) : myPack (dropWhile (== x) xs)

-- Problem 10
-- Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
myEncode :: Eq a => [a] -> [(Int, a)]
myEncode = map (\x -> (length x, head x)) . myPack
