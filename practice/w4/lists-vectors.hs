--01
insertAt :: Int -> a -> [a] -> [a]
insertAt 0 x []         = [x]
insertAt _ _ []          = error "No such position"
insertAt 0 x xs         = x : xs
insertAt n x (y : ys)   = y : insertAt (n-1) x xs
--02
sublistBetween :: Int -> Int -> [a] -> [a]
sublistBetween start end xs = take (end - start) (drop start xs)
--03
chuncksOf :: Int -> [a] -> [[a]]
chuncksOf size xs = if lenght xs <= size then [xs] else take size xs : chuncksOf size (drop size xs)
--04
isSorted :: [Int] -> Bool 
isSorted []             = True
isSorted [_]            = True
isSorted (x1 : x2 : xs) = x1 < x2 && isSorted (x2 : xs)
--05
isAscending :: Int -> Bool
isAscending num = isSorted (reverse (numIntoList num))

numIntoList :: Int -> [Int]
numIntoList n
    | n < 10 = [n]
    | otherwise = (n `mod` 10) : numIntoList (n `div` 10)
--06
merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) 
    | x <= y = x : (merge xs (y:ys))
    | othwerwise  = y : (merge (x:xs) ys)

-- as -pattern @
merge' :: [Int] -> [Int] -> [Int]
merge' xs [] = xs
merge' [] ys = ys
merge' left@(x:xs) right@(y:ys) 
    | x <= y = x : (merge' xs right@)
    | othwerwise  = y : (merge' left@ ys)

main :: IO()
main = do
    print(isAscending 123)