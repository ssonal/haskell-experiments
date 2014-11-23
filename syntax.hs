signum' x = 
    if x < 0
        then -1
        else if x > 0
            then 1
            else 0

weirdFun x = 
    case x of 
        0 -> 1
        1 -> 5
        2 -> 2
        _ -> -1

roots a b c =
    let det = sqrt (b * b - 4 * a * c)
        twice_a = 2 * a
    in  ((-b + det) / twice_a,
         (-b - det) / twice_a)

firstElement :: [a] -> Maybe a
firstElement []     = Nothing
firstElement (x:xs) = Just x

data Color a = RGB a a a deriving (Show)

addColor :: (Num a) => Color a -> Color a -> Color a
addColor (RGB a b c) (RGB x y z) = RGB (a + x) (b + y) (c + z)