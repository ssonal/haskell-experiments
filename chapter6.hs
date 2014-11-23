-- Chapter 6 of Learn You a Haskell

import Data.List as L
import Data.Map as M
import Data.Set as S
import Data.Char as C

numUniques :: (Eq a) => [a] -> Int
numUniques = length . L.nub

wordNums :: String -> [(String, Int)]
wordNums = Prelude.map (\ws -> (head ws, length ws)) . L.group . L.sort . L.words

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = Prelude.foldl (\accum b -> if needle `L.isPrefixOf` b then True else accum) False (L.tails haystack)

encode :: Int -> String -> String
encode n = Prelude.map (C.chr . (+ n) . ord) 
--encode n m = Prelude.map (\ch -> C.chr $ C.ord ch + n) m

decode :: Int -> String -> String
decode n = encode (negate n)

digitSum :: Int -> Int
digitSum = sum . Prelude.map C.digitToInt . show

firstTo :: Int -> Maybe Int
firstTo n = L.find (\x -> digitSum x == n) [1..]

phoneBook :: M.Map String String
phoneBook = M.fromList $
    [ ("betty", "555-5555")
    , ("bonnie", "352-8787")
    , ("willson", "626-2376")
    , ("dennis", "123-4567")
    , ("penny", "987-6543")]

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key []              = Nothing
findKey key ((k, v):xs)     
    | k == key              = Just v
    | otherwise             = findKey key xs

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key xs = L.foldl (\acc (k, v) -> if k == key then Just v else acc) Nothing xs
