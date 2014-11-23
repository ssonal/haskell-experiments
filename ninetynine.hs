--import Data.List

-- Problem 1
myLast :: [a] -> a
myLast []       = error "There is no last element for an empty list."
myLast (x:[])   = x
myLast (_:xs)   = myLast xs
-- Using higher order functions
-- myLast xs       = foldl1 (\ acc x -> x) xs

-- Problem 2
myButLast :: [a] -> a
myButLast xs
    | length xs < 2 = error "Only works for lists of at least 2 elements."
myButLast (x:_:[])  = x
myButLast (_:xs)    = myButLast xs
-- myButLast = tail . init

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt [] _                  = error "Invalid index"
elementAt (x:xs) n
    | n <= 0                    = error "Invalid index"
    | n == 1                    = x
    | otherwise                 = elementAt xs (n - 1)

-- Problem 4
myLength :: [a] -> Int
-- myLength []     = 0
-- myLength (x:xs) = 1 + myLength xs
myLength = foldl (\accum _ -> 1 + accum) 0

-- Problem 5
myReverse :: [a] -> [a]
--myReverse []        = []
--myReverse (x:xs)    = myReverse xs ++ [x]
myReverse = foldl (\accum x -> x : accum) []

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome []             = True
isPalindrome [_]            = True
isPalindrome xs
    | head xs /= last xs    = False
    | otherwise             = isPalindrome . init $ tail xs

-- Problem 7
-- postpone implementing myFlatten because it requires reading about creating new Data Types in Haskell

-- Problem 8
myCompress :: (Eq a) => [a] -> [a]
myCompress (x:xs) = foldl (\accum y -> if y == last accum then accum else accum ++ [y]) [x] xs

-- Problem 9
pack :: [a] -> [[a]]
pack [] = []

fib :: Int -> Int
fib n
    | n < 1     = error "Invalid index"
    | n == 1    = 1
    | n == 2    = 2
    | otherwise = fib (n - 1) + fib (n - 2)

--numUniques :: (Eq a) => [a] -> Int
--numUniques = length . nub 
