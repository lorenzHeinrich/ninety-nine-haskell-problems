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
myLength []     = 0
myLength [a]    = 1
myLength (a:as) = 1 + myLength as

-- alternative solution using foldl
myLength' :: [a] -> Int
myLength' = foldl (\i a -> i + 1) 0

-- Problem 5
-- Reverse a list.
-- 
 -- λ> myReverse "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
-- λ> myReverse [1,2,3,4]
-- [4,3,2,1]

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (a:as) = myReverse as ++ [a]

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

-- Problem 6
-- Find out whether a list is a palindrome.
-- Hint: A palindrome can be read forward or backward; e.g. (x a m a x).
--
-- Example in Haskell:
-- λ> isPalindrome [1,2,3]
-- False
-- λ> isPalindrome "madamimadam"
-- True
-- λ> isPalindrome [1,2,4,8,16,8,4,2,1]
-- True
isPalindrome :: Eq a => [a] -> Bool
isPalindrome []  = True
isPalindrome [_] = True
isPalindrome xs  = head xs == last xs
                   && isPalindrome (tail $ init xs)

-- Problem 7
-- Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
--
-- Example:
-- * (my-flatten '(a (b (c d) e)))
-- (A B C D E)
--
-- Example in Haskell:
-- λ> flatten (Elem 5)
-- [5]
-- λ> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- [1,2,3,4,5]
-- λ> flatten (List [])
-- []
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a)  = [a]
flatten (List as) = foldl (\xs x -> xs ++ flatten x) [] as

flatten' :: NestedList a -> [a]
flatten' (Elem x)  = [x]
flatten' (List xs) = concatMap flatten xs

-- this uses the same mechanic as concatMap
flatten'' :: NestedList a -> [a]
flatten'' (Elem x)      = [x]
flatten'' (List (x:xs)) = flatten x ++ flatten (List xs)

-- Problem 8
-- Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced with a single copy of the element.
-- The order of the elements should not be changed.
-- 
-- Example:
-- * (compress '(a a a a b c c a a d e e e e))
-- (A B C A D E)
--
-- Example in Haskell:
-- λ> compress "aaaabccaadeeee"
-- "abcade"
compress :: Eq a => [a] -> [a]
compress (x:y:xs)
    | x == y    = compress (y:xs)
    | otherwise = [x,y] ++ compress xs
compress xs = xs

-- Problem 9
-- Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists.
-- 
-- Example:
-- * (pack '(a a a a b c c a a d e e e e))
-- ((A A A A) (B) (C C) (A A) (D) (E E E E))
--
-- Example in Haskell:
-- λ> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]
pack :: Eq a => [a] -> [[a]]
pack xs = foldl combiner [] xs
    where
        combiner [] x = [[x]]
        combiner xs x
            | last (last xs) == x = init xs ++ [x : last xs]
            | otherwise = xs ++ [[x]]

-- Problem 10
-- Run-length encoding of a list.
--
-- Use the result of Problem 9 to implement the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
-- 
-- Example:
-- * (encode '(a a a a b c c a a d e e e e))
-- ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
--
-- Example in Haskell:
-- λ> encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) (pack xs)

-- Problem 11
-- Modified run-length encoding.

-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list.
-- Only elements with duplicates are transferred as (N E) lists.
-- Example:
-- * (encode-modified '(a a a a b c c a a d e e e e))
-- ((4 A) B (2 C) (2 A) D (4 E))
--
-- Example in Haskell:
-- λ> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']

data RunLengthEncoding a = Multiple Int a | Single a
instance Show a => Show (RunLengthEncoding a) where
  show (Single x)   = "Single " ++ show x
  show (Multiple n x) = "Mutliple " ++ show n ++ " " ++ show x

encodeModified :: Eq a => [a] -> [RunLengthEncoding a]
encodeModified xs = map (\(n, x) -> if n == 1 then Single x else Multiple n x) (encode xs)

-- Problem 12
-- Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

-- Example in Haskell:
-- λ> decodeModified 
--        [Multiple 4 'a',Single 'b',Multiple 2 'c',
--         Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"
decodeModified :: [RunLengthEncoding a] -> [a]
decodeModified ((Single x):xs)   = x : decodeModified xs
decodeModified ((Multiple n x):xs) = replicate n x ++ decodeModified xs

-- Problem 13
-- Run-length encoding of a list (direct solution).
--
-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them.
-- As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
--
-- Example:
-- * (encode-direct '(a a a a b c c a a d e e e e))
-- ((4 A) B (2 C) (2 A) D (4 E))
--
-- Example in Haskell:
-- λ> encodeDirect "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']
encodeDirect :: Eq a => [a] -> [RunLengthEncoding a]
encodeDirect []     = []
encodeDirect (x:xs) = packOrPrepend x (encodeDirect xs)
    where
        packOrPrepend x [] = [Single x]
        packOrPrepend x (s@(Single y):ys)
            | x == y    = Multiple 2 y:ys
            | otherwise = Single x : s : ys
        packOrPrepend x (m@(Multiple n y):ys)
            | x == y    = Multiple (n + 1) y : ys
            | otherwise =  Single x : m : ys

-- Problem 14
-- Duplicate the elements of a list.
--
-- Example:
-- * (dupli '(a b c c d))
-- (A A B B C C C C D D)
--
-- Example in Haskell:
-- λ> dupli [1, 2, 3]
-- [1,1,2,2,3,3]
dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = x : x : dupli xs

dupli' :: [a] -> [a]
dupli'= concatMap (\x -> [x,x])

-- Problem 15
-- Replicate the elements of a list a given number of times.
-- Example:
-- * (repli '(a b c) 3)
-- (A A A B B B C C C)
--
-- Example in Haskell:
-- λ> repli "abc" 3
-- "aaabbbccc"
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-- Problem 16
-- Drop every N'th element from a list.
--
-- Example:
-- * (drop '(a b c d e f g h i k) 3)
-- (A B D E G H K)
--
-- Example in Haskell:
-- λ> dropEvery "abcdefghik" 3
-- "abdeghk"
dropEvery :: [a] -> Int -> [a]
dropEvery [] n = []
dropEvery xs n = take (n-1) xs ++ dropEvery (drop n xs) n

-- Problem 17
-- Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.
-- Example:
-- * (split '(a b c d e f g h i k) 3)
-- ( (A B C) (D E F G H I K))
-- Example in Haskell:
-- λ> split "abcdefghik" 3
-- ("abc", "defghik")
split :: [a] -> Int -> ([a], [a])
-- split xs n = (take n xs, drop n xs)
split xs n = splitAt n xs

-- Problem 18
-- Extract a slice from a list.
-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included).
-- Start counting the elements with 1.
-- Example:
-- * (slice '(a b c d e f g h i k) 3 7)
-- (C D E F G)
-- Example in Haskell:
-- λ> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"
slice :: [a] -> Int -> Int -> [a]
-- slice xs i k = fst (splitAt (k-i) (drop (i-1) xs))
slice xs i k = take (k-i) (drop (i-1) xs)

-- Problem 19
-- Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).
--
-- Examples:
-- * (rotate '(a b c d e f g h) 3)
-- (D E F G H A B C)
-- * (rotate '(a b c d e f g h) -2)
-- (G H A B C D E F)
--
-- Examples in Haskell:
-- λ> rotate ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"
-- λ> rotate ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"
rotate :: [a] -> Int -> [a]
rotate xs n =
    let i = (length xs + n) `mod` length xs
    in drop i xs ++ take i xs

-- Problem 20
-- Remove the K'th element from a list.

-- Example in Prolog:
-- ?- remove_at(X,[a,b,c,d],2,R).
-- X = b
-- R = [a,c,d]

-- Example in Lisp:
-- * (remove-at '(a b c d) 2)
-- (A C D)
-- (Note that this only returns the residue list, while the Prolog version also returns the deleted element.)

-- Example in Haskell:
-- λ> removeAt 2 "abcd"
-- ('b',"acd")
removeAt :: Int -> [a] -> (a, [a])
removeAt i xs = (xs !! (i-1), take (i-1) xs ++ drop i xs)

-- Problem 21
-- Insert an element at a given position into a list.

-- Example:
-- * (insert-at 'alfa '(a b c d) 2)
-- (A ALFA B C D)
-- Example in Haskell:
-- λ> insertAt 'X' "abcd" 2
-- "aXbcd"
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs i = front ++ [x] ++ back
    where (front, back) = splitAt (i-1) xs

-- Problem 22
-- Create a list containing all integers within a given range.
--
-- Example:
-- * (range 4 9)
-- (4 5 6 7 8 9)
-- Example in Haskell:
-- λ> range 4 9
-- [4,5,6,7,8,9]
range :: Int -> Int -> [Int]
range a b = [a..b]


-- skip Problem 23 - 25 because they require imports

-- Problem 26
-- Generate combinations of K distinct objects chosen from the N elements of a list.

-- In how many ways can a committee of 3 be chosen from a group of 12 people?
-- We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients).
-- For pure mathematicians, this result may be great.
-- But we want to really generate all the possibilities in a list.

-- Example:

-- * (combinations 3 '(a b c d e f))
-- ((A B C) (A B D) (A B E) ... )
-- Example in Haskell:

-- λ> combinations 3 "abcdef"
-- ["abc","abd","abe",...]

combinations :: Int -> [a] -> [[a]]
combinations 0 xs = [[]]
combinations k l@(x:xs)
    | k == length l = [l]
    | otherwise     = map (x :) (combinations (k-1) xs) ++ combinations k xs

-- Problem 27
-- Group the elements of a set into disjoint subsets.
--
-- a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons?
-- Write a function that generates all the possibilities and returns them in a list.
--
-- Example:
-- * (group3 '(aldo beat carla david evi flip gary hugo ida))
-- ( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) )
-- ... )
group3 :: Eq a => [a] -> [[[a]]]
group3 xs = addThrees (combinations 4 xs) xs
    where
        -- Überprüft, ob ein Element in der Liste ist
        isIn :: Eq a => [a] -> a -> Bool
        isIn [] _     = False
        isIn (x:xs) y = x == y || isIn xs y

        -- filter ys for elements not in xs --
        filtered :: Eq a => [a] -> [a] -> [a]
        filtered xs = filter (not . isIn xs)

        addThrees :: Eq a => [[a]] -> [a] -> [[[a]]]
        addThrees fours xs = concatMap (\four -> addThreesToFour four (combinations 3 (filtered four xs)) xs) fours

        addThreesToFour :: Eq a => [a] -> [[a]] -> [a] -> [[[a]]]
        addThreesToFour _ [] xs                = []
        addThreesToFour four (three:threes) xs = [rest four three xs, three, four] : addThreesToFour four threes xs
            where 
                rest :: Eq a => [a] -> [a] -> [a] -> [a]
                rest four three xs = filtered four (filtered three xs)

-- b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.

-- Example:
-- * (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
-- ( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) )
-- ... )
-- Note that we do not want permutations of the group members; i.e. ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO) ...). However, we make a difference between ((ALDO BEAT) (CARLA DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).

-- You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".
-- Example in Haskell:

-- λ> group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
-- (altogether 1260 solutions)

-- λ> group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
-- (altogether 756 solutions)

group :: Eq a => [Int] -> [a] -> [[[a]]] 
group [first, second, third] xs
    | length xs == first + second + third = 
        concatMap (\f -> addToFirst f (combinations second (filtered f xs)) xs) (combinations first xs)
    | otherwise = error "sum of group sizes must match list length"
    where
        addToFirst :: Eq a => [a] -> [[a]] -> [a] -> [[[a]]]
        addToFirst _ [] _           = []
        addToFirst first (second:seconds) xs = 
            [first, second, rest first second xs] : addToFirst first seconds xs
            where 
                rest first second xs = filtered first (filtered second xs)

        -- Überprüft, ob ein Element in der Liste ist
        isIn :: Eq a => [a] -> a -> Bool
        isIn [] _     = False
        isIn (x:xs) y = x == y || isIn xs y

        -- filter ys for elements not in xs --
        filtered :: Eq a => [a] -> [a] -> [a]
        filtered xs = filter (not . isIn xs)

