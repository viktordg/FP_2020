import Data.List(nub)
--------------------------------------------------------------------------------------
sumDigits :: Int -> Int
sumDigits n = if n < 10 then n else n `mod` 10 + sumDigits (n `div` 10) 

isInteresting :: Int -> Bool
isInteresting n = n `mod` (sumDigits n) == 0
--------------------------------------------------------------------------------------
hasSix :: Int -> Bool
hasSix n = if n `mod` 10 == 6 then True else n `div` 10 /= 0 && hasSix (n `div` 10)

getSum :: Int -> Int -> Int
getSum a b = sum [x | x <- [a..b], hasSix x]
--------------------------------------------------------------------------------------
fac :: Double -> Double
fac n = product [1..n]

sinSum :: Double -> Double -> Double
sinSum n x = helper n x 0 0.0
    where
        helper n x i r
            | i == n    = r + (((-1) ** i) * (n ** ( (2 * i) + 1 ))) /  fac((2 * i) + 1)
            | i < n     = helper n x (i + 1) (r + (((-1) ** i) * (n ** ( (2 * i) + 1 ))) / fac((2 * i) + 1))
--------------------------------------------------------------------------------------
myCompare :: [Int] -> [Int] -> Bool
myCompare [] []           = True
myCompare [x] [y]         = if x >= y then True else False
myCompare (x:xs) (y:ys)   = if x >= y then myCompare xs ys else False


dominates :: (Int -> Int) -> (Int -> Int) -> [Int] -> Bool
dominates f g [] = False
dominates f g xs = myCompare (map f xs) (map g xs)
--------------------------------------------------------------------------------------
countOccurances :: Int -> [Int] -> Int
countOccurances x [] = 0
countOccurances x (y:ys) = if x == y then 1 + countOccurances x ys else countOccurances x ys 

sumUniqueInList :: [Int] -> [Int] -> Int
sumUniqueInList [] []       = 0
sumUniqueInList xs []       = 0
sumUniqueInList xs [y]      = if countOccurances y xs > 1 then 0 else y
sumUniqueInList xs (y:ys)   = if countOccurances y xs > 1 then sumUniqueInList xs ys else y + sumUniqueInList xs ys

sumUnique :: [[Int]] -> Int
sumUnique []        = 0
sumUnique (x:xs)    = sumUniqueInList x (nub x) + sumUnique xs
--------------------------------------------------------------------------------------
main :: IO()        
main = do
    print(isInteresting 410)
    print("--------")
    print(getSum 4 28)
    print("--------")
    print(sinSum 5 5)
    print("--------")
    print(dominates (+4) (*2) [1..4])
    print("--------")
    print(sumUnique [[1,2,3,2],[-4,-4],[5]])
    print(sumUnique [[2,2,2],[3,3,3],[4,4,4]])
    print(sumUnique [[1,2,3],[4,5,6],[7,8,9]])
    

    
    