module Questions where

import Data.List
import System.Random

-- Problem 1
-- Find the last element of a list.

myLast :: [a] -> a
myLast [x] = x
myLast (_ : xs) = myLast xs

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
-- Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be
-- placed in separate sublists.
myPack :: Eq a => [a] -> [[a]]
myPack [] = []
myPack (x : xs) = (x : takeWhile (== x) xs) : myPack (dropWhile (== x) xs)

-- Problem 10
-- Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data
-- compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of
-- duplicates of the element E.
myEncode :: Eq a => [a] -> [(Int, a)]
myEncode = fmap (\x -> (length x, head x)) . myPack

-- Problem 11
-- Modified run-length encoding. Modify the result of problem 10 in such a way that if an element has no duplicates it
-- is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
data Encoded a = Multiple Int a | Single a deriving (Show)

myEncodeModified :: Eq a => [a] -> [Encoded a]
myEncodeModified = fmap (\(x, y) -> if x == 1 then Single y else Multiple x y) . myEncode

-- Problem 12
-- Decode a run-length encoded list. Given a run-length code list generated as specified in problem 11. Construct its
-- uncompressed version.
myDecodeModified :: Eq a => [Encoded a] -> [a]
myDecodeModified = concatMap merge
  where
    merge (Multiple n x) = replicate n x
    merge (Single x) = [x]

-- Problem 13
-- Run-length encoding of a list (direct solution). Implement the so-called run-length encoding data compression method
-- directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them.
-- As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
-- See problem 11

-- Problem 14
-- Duplicate the elements of a list.
myDuplicate :: [a] -> [a]
myDuplicate [x] = [x, x]
myDuplicate (x : xs) = [x, x] ++ myDuplicate xs

-- Problem 15
-- Replicate the elements of a list a given number of times.
myReplicate :: [a] -> Int -> [a]
myReplicate [x] n = take n (repeat x)
myReplicate (x : xs) n = take n (repeat x) ++ myReplicate xs n

-- Problem 16
-- Drop every N'th element from a list.
myDropEvery :: [a] -> Int -> [a]
myDropEvery [] _ = []
myDropEvery xs n = take (n - 1) xs ++ myDropEvery (drop (length (take n xs)) xs) n

-- Problem 17
-- Split a list into two parts; the length of the first part is given. Do not use any predefined predicates.
mySplit :: [a] -> Int -> ([a], [a])
mySplit xs n = (take n xs, drop n xs)

-- Problem 18
-- Extract a slice from a list. Given two indices, i and k, the slice is the list containing the elements between the
-- i'th and k'th element of the original list (both limits included). Start counting the elements with 1.
mySlice :: [a] -> Int -> Int -> [a]
mySlice xs n m = drop (n - 1) $ take m xs

-- Problem 19
-- Rotate a list N places to the left.
myRotate :: [a] -> Int -> [a]
myRotate xs n
  | n >= 0 = drop n xs ++ take n xs
  | otherwise = drop modifiedLength xs ++ drop n (take modifiedLength xs)
  where
    modifiedLength = length xs + n

-- Problem 20
-- Remove the K'th element from a list.
myRemoveAt :: Int -> [a] -> (a, [a])
myRemoveAt n xs = (myElementAt xs n, take (n - 1) xs ++ drop n xs)

-- Problem 21
-- Insert an element at a given position into a list.
myInsertAt :: a -> [a] -> Int -> [a]
myInsertAt y xs n = take (n - 1) xs ++ [y] ++ drop (n - 1) xs

-- Problem 22
-- Create a list containing all integers within a given range.
myRange :: Int -> Int -> [Int]
myRange n m = [n .. m]

-- Problem 23
-- Extract a given number of randomly selected elements from a list.
myRandomSelect :: [a] -> Int -> IO [a]
myRandomSelect xs n = do
  gen <- getStdGen
  return $ take n [xs !! x | x <- randomRs (0, length xs - 1) gen]

-- Problem 24
-- Lotto: Draw N different random numbers from the set 1..M.
myDiffSelect :: (Random a, Num a) => Int -> a -> IO [a]
myDiffSelect n m = do
  gen <- getStdGen
  return $ take n [x | x <- randomRs (0, m) gen]

myRandomPermutation :: [a] -> IO [a]
myRandomPermutation xs = do
  myElementAt (permutations xs) . fst . random <$> getStdGen

-- Problem 26
-- (**) Generate the combinations of K distinct objects chosen from the N elements of a list
-- In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220
-- possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be
-- great. But we want to really generate all the possibilities in a list.
myCombinations :: Int -> [a] -> [[a]]
myCombinations n = filter ((n ==) . length) . subsequences

-- Problem 27
-- Group the elements of a set into disjoint subsets.
-- a) In how many ways can a group of 9 people work in 3 disjoint
-- subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.
-- b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a
-- list of groups.
myGroup :: (Foldable t, Eq a) => t Int -> [a] -> [[[a]]]
myGroup ns xss = concatMap (\x -> myCombinations x (group xss)) ns

-- Problem 28
-- Sorting a list of lists according to length of sublists
-- a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this
-- list according to their length. E.g. short lists first, longer lists later, or vice versa.
-- b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort
-- the elements of this list according to their length frequency; i.e., in the default, where sorting is done
-- ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.
myLSort :: [[a]] -> [[a]]
myLSort = sortOn length

myCount :: Eq a => a -> [a] -> Int
myCount x = length . filter (== x)

myLFSort :: (Ord (t a), Foldable t) => [t a] -> [t a]
myLFSort xss = [y | (_, y) <- sort withCount]
  where
    withLength = fmap (\x -> (length x, x)) xss
    withCount = fmap (\(x, y) -> (myCount x [x | (x, _) <- withLength], y)) withLength
