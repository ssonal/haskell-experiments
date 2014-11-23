-- Chapter 7
module Chapter7
( Point (..)
, Shape (..)
, area
, nudge
, baseCircle
, baseRectangle
) where

import qualified Data.Map as M

data Shape  = Circle Point Float 
            | Rectangle Point Point deriving (Show)

data Point = Point Float Float deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x1 - x2) * (abs $ y1 - y2)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) x_d y_d                    = Circle (Point (x + x_d) (y + y_d)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) x_d y_d   = Rectangle (Point (x1 + x_d) (y1 + y_d)) (Point (x2 + x_d) (y2 + y_d))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRectangle :: Float -> Float -> Shape
baseRectangle w h = Rectangle (Point 0 0) (Point w h)

--data Person =   Person { firstName :: String
--                       , lastName :: String
--                       , age :: Int
--                       , height :: Float
--                       , phoneNumber :: String
--                       , flavor :: String } deriving (Show)

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector a b c) `vplus` (Vector x y z) = Vector (a + x) (b + y) (c + z)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector a b c) `dotProd` (Vector x y z) = (a * x) + (b * y) + (c * z)

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector x y z) `vmult` n = Vector (x * n) (y * n) (z * n)

data Person =   Person { firstName :: String
                       , lastName :: String
                       , age :: Int 
                       } deriving (Show, Read, Eq)

willson = Person { firstName = "Willson", lastName = "Mock", age = 28 }
cheryl  = Person { firstName = "Cheryl", lastName = "Chan", age = 27 }
dennis  = Person { firstName = "Dennis", lastName = "Li", age = 29 }

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- type synonyms
type Name = String
type PhoneNumber = String
type PhoneBook = [(Name, PhoneNumber)]

phonebook :: PhoneBook
phonebook = 
    [ ("Willson", "321-626-2376")
    , ("Dennis", "919-432-0785")
    , ("Cheryl", "212-640-0528")]

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = M.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockNumber mp = case M.lookup lockNumber mp of
    Nothing             -> Left $ "Locker " ++ show lockNumber ++ " doesn't exist!"
    Just (state, code)  -> if state /= Taken 
                            then Right code
                            else Left $ "Locker " ++ show lockNumber ++ " is already taken!"

lockers :: LockerMap
lockers = M.fromList
    [ (100, (Taken, "ZD39I"))
    , (101, (Free, "JAH3I"))
    , (103, (Free, "IQSA9"))
    , (105, (Free, "QOTSA"))
    , (109, (Taken, "893JJ"))
    , (110, (Taken, "99292"))
    ]

-- recursive data structures
--data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty ^++ ys        = ys
(x :-: xs) ^++ ys   = x :-: (xs ^++ ys)

-- binary search trees
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree              = singleton x
treeInsert x (Node a left right)
    | x == a                        = Node x left right
    | x < a                         = Node a (treeInsert x left) right
    | x > a                         = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree            = False
treeElem x (Node a left right)
    | x == a                    = True
    | x < a                     = treeElem x left
    | x > a                     = treeElem x right

nums = [8,6,4,1,7,3,5]
numsTree = foldr treeInsert EmptyTree nums

-- type class part 2
data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Yellow == Yellow = True
    Green == Green = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red Light"
    show Yellow = "Yellow Light"
    show Green = "Green Light"

class YesNo a where
    yesno :: a -> Bool

instance YesNo Bool where
    yesno = id

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _  = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _   = True

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _         = True

instance YesNo (Maybe a) where
    yesno Nothing   = False
    yesno (Just _)  = True

yesnoif :: (YesNo y) => y -> a -> a -> a
yesnoif x truevalue falsevalue =
    if yesno x
        then truevalue
        else falsevalue