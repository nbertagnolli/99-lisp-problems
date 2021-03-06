-- This module holds solutions to the 99 lisp problems
-- This website has some nice scala implementations
-- http://aperiodic.net/phil/scala/s-99/
-- To run the code stack ghci
-- :load Haskell99LispProblems
module Haskell99LispProblems where

import Data.Tree
import Data.List (group)

-- Problem 1
-- Find the last element of a list.
firstElem :: [Integer] -> Integer
firstElem (x : []) = x
firstElem (x : xs) = x

lastElem :: [Integer] -> Integer
lastElem (x : []) = x
lastElem (x : xs) = lastElem xs


-- Problem 2
-- Find the last but one element of a list.
lastButOne :: [Integer] -> Integer
lastButOne (x : y : []) = x
lastButOne (x : xs) = lastButOne xs


-- Problem 3
-- Find the Kth element of a list.
nthElement :: Int -> [Int] -> Int
nthElement i ls
  | i > (length ls) = error "i bigger than list"
  | i == 0          = head ls
  | otherwise       = nthElement (i - 1) (tail ls)
-- -- nthElement _ [] = error "empty list"
-- -- nthElement i (x : xs) = if i == 0  then x else
-- -- nthElement i (x : xs) = if i > length xs then error "i is too big" else nthElement i - 1 xs


-- Problem 4
-- Find the number of elements of a list.
nElements :: [Integer] -> Integer
nElements [] = 0
nElements (x : xs) = 1 + nElements xs


-- Problem 5
reverseList :: [Integer] -> [Integer]
reverseList (x:[]) = [x] ++ []
reverseList (x: xs) = reverseList xs ++ [x]


-- Problem 6
-- Check if a list is a palindrome
isPalindromeHelper :: [Integer] -> [Integer] -> Bool
isPalindromeHelper (x:xs) (y:ys)
  | x == y && xs == [] && ys == [] = True
  | x == y    = isPalindromeHelper xs ys
  | x /= y    = False
  | otherwise = False

isPalindrome :: [Integer] -> Bool
isPalindrome ls = isPalindromeHelper ls reversed
  where reversed = reverse ls

-- Problem 7
-- Flatten a nested list
data NestedList a = Elem a | List [NestedList a]
-- This data type is equivalent to Data.Tree see∷
-- http://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Tree.html#t:Forest
flattenList :: NestedList a -> [a]
flattenList (Elem x) = [x]
flattenList (List x) = concat $ map flattenList x


-- Problem 8
-- Eliminate consecutive duplicates of list elements.
compressList :: (Eq a) => [a] -> [a]
compressList (x1:x2:xs)
  | x1 /= x2 && xs == [] = [x1, x2]
  | x1 == x2 && xs == [] = [x2]
  -- If x1 and x2 are the same just recurse on the list + x2
  | x1 == x2 = compressList(x2 : xs)
  -- If they are different keep x1 and continue to recurse on x2 + list
  | x1 /= x2 = x1 : compressList(x2 : xs)

-- So much better!
compress :: Eq a => [a] -> [a]
compress = map head . group

-- Problem 9
-- Pack consecutive duplicates of list elements into sublists.
-- packListCheat :: (Eq a) => [a] ->  [[a]]
packListCheat xs = group xs

packList :: (Eq a) => [a] ->  [[a]]
packList [] = []
packList (x1:xs) = first : packList second
  where currentSpan = span (== x1) $ x1:xs
        first = fst $ currentSpan
        second = snd $ currentSpan


-- Problem 10
-- Run-length encoding of a list.
-- Use the result of problem P09 to implement the so-called run-length encoding
-- data compression method. Consecutive duplicates of elements are encoded as
-- tuples (N, E) where N is the number of duplicates of the element E.
runLengthEncoding :: (Eq a) => [a] -> [(a, Int)]
runLengthEncoding = map (\x -> (head x, length x)) . packList


-- Problem 11
-- Modified run-length encoding.
-- Modify the result of problem P10 in such a way that if an element has no
-- duplicates it is simply copied into the result list. Only elements with
-- duplicates are transferred as (N, E) terms.
-- modRunLengthEncoding :: (Eq a)
