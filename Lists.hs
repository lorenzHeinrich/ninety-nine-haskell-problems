-- Problem 1
-- Find the last element of a list.
--
-- Example in Haskell:
-- λ> myLast [1,2,3,4]
-- 4
-- λ> myLast ['x','y','z']
-- 'z'
myLast :: [a] -> a
-- we can use Eta reduce, to factor out the right most common argument
myLast = last

-- Problem 2
-- Find the last-but-one (or second-last) element of a list.
-- 
-- Example in Haskell:
-- λ> myButLast [1,2,3,4]
-- 3
-- λ> myButLast ['a'..'z']
-- 'y'
myButLast :: [a] -> a
-- function composition with eta reduce : equivalent to last (init a)
myButLast = last . init

-- alternative solution
-- !! operator is used to access a list at a given index
myButLast' a = reverse a !! 1

-- Problem 3
-- Find the K'th element of a list.
-- 
-- The first element in the list is number 1. Example:
-- * (element-at '(a b c d e) 3)
-- c
elementAt :: [a] -> Int -> a
-- !! uses zero-indexing
elementAt a i = a !! (i-1)

-- Problem 4
-- Find the number of elements in a list.
--
-- Example in Haskell:
-- λ> myLength [123, 456, 789]
-- 3
-- λ> myLength "Hello, world!"
-- 13
myLength :: [a] -> Int
myLength [] = 0
myLength [a] = 1
myLength (a:as) = 1 + myLength as

-- alternative solution using foldl
myLength' :: [a] -> Int
myLength' = foldl (\i a -> i + 1) 0
