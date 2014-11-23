doubleMe :: (Num a) => a -> a
doubleMe x = x + x

--doubleUs x y = x * 2 + y * 2
doubleUs :: (Num a) => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber :: Int -> Int
doubleSmallNumber x = if x > 100
                        then x
                        else x * 2

doubleSmallNumber' :: Int -> Int
doubleSmallNumber' x = (if x > 100
                        then x
                        else x * 2) + 1

conanO'Brien :: String
conanO'Brien = "It's a me, Conan O' Brien"

boomBangs :: [Int] -> [String]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

--factorial :: Integer -> Integer
--factorial n = product [1..n]

myDrop :: Int -> [a] -> [a]
myDrop num xs = if num <= 0 || null xs
                then xs
                else myDrop (num - 1) (tail xs)

-- Chapter 3
lucky :: Int -> [Char]
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're outta luck, pal!"

sayMe :: Int -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe x = "Not between 1 and 5"

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

myLast :: [a] -> a
myLast [] = error "myLast can't be applied to an empty list."
myLast (x:[]) = x
myLast (_:xs) = myLast xs

initials :: String -> String -> String
initials (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

-- Chapter 4
replicate' :: Int -> a -> [a]
replicate' n v
    | n <= 0    = []
    | otherwise = v : replicate' (n - 1) v

take' :: Int -> [a] -> [a]
take' n _
    | n <= 0    = []
take' _ []      = []
take' n (x:xs)  = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _           = []
zip' _ []           = []
zip' (x:xs) (y:ys)  = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ []      = False
elem' v (x:xs)
    | v == x    = True
    | otherwise = elem' v xs

quicksort :: (Ord a) => [a] -> [a]
quicksort []            = []
quicksort (p:xs)        = 
    let smallerOrEqual  = [x | x <- xs, x <= p]
        larger          = [x | x <- xs, x > p]
    in quicksort smallerOrEqual ++ [p] ++ quicksort larger

-- Chapter 5
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWidth' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWidth' _ [] _            = []
zipWidth' _ _ []            = []
zipWidth' f (x:xs) (y:ys)   = f x y : zipWidth' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ []       = []
map' f (x:xs)   = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []        = []
filter' f (x:xs)
    | f x           = x : filter' f xs
    | otherwise     = filter' f xs

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ accum []       = accum
foldl' f accum (x:xs)   = foldl' f (f accum x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ accum []       = accum
foldr' f accum (x:xs)   = f x (foldr' f accum xs)

sum' :: (Num a) => [a] -> a
sum' = foldr' (+) 0

map2 :: (a -> b) -> [a] -> [b]
map2 f xs = foldr' (\x acc -> f x : acc) [] xs

elem2 :: (Eq a) => a -> [a] -> Bool
elem2 x xs = foldr (\y acc -> if y == x then True else acc) False xs

-- example of function composition
fn = ceiling . negate . tan . cos . max 50
