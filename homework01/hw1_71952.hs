--problem_01--------------------------------------------------------------------
findSum :: Int -> Int -> Int -> Int
findSum a b n = helper a b n 0 0 
    where
        helper a b n p r
            | n < 3         = error "n must be >3"
            | p <= n - 2    = r + (b * (2^p)) + helper a b n (p + 1) r
            | p == n - 1    = r + a + (b * (2^p)) + helper a b n (p + 1) r + (r - (b * (2^(p-1)))) - (r - (2 * (b * (2^p)))) + 4
            | otherwise     = r
--problem_02--------------------------------------------------------------------
isSquare :: Int -> Bool
isSquare n = helper n 1
    where 
        helper n i
            | i * i == n                        = True
            | i * i /= n && i > (n `div` 2)     = False
            | otherwise                         = helper n (i + 1)
--problem_03--------------------------------------------------------------------
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n 
    | (length [x | x <- [2 .. (n `div` 2)], n `mod` x == 0]) > 0    = False
    | otherwise                                                     = True

getNumberToCheck :: Integer -> Integer -> Integer
getNumberToCheck n k = n `mod` (10^k)

countRemainingDigits :: Integer -> Integer
countRemainingDigits n
    | n <=9         = 1
    | otherwise     = 1 + countRemainingDigits(n `div` 10)

isSpecial :: Integer -> Integer -> Bool
isSpecial n k 
    | isPrime(getNumberToCheck n k) == False    = False
    | countRemainingDigits n == k               = isPrime(getNumberToCheck n k)
    | otherwise                                 = isSpecial (n `div` 10) k
--------------------------------------------------------------------------------
main :: IO()
main = do
    --problem_01----------------------------
    print(findSum 0 2 10)
    print(findSum 5 3 5) 
   -- print(findSum 1 2 2)
    --problem_02----------------------------
    print(isSquare 1)
    print(isSquare 2)
    print(isSquare 4)
    print(isSquare 17)
    print(isSquare 256)
    print(isSquare 2500)
    --problem_03----------------------------
    print(isSpecial 131 2)     -- → True (числата 13 и 31 са прости)
    print(isSpecial 472 2)     -- → False (47 е просто число, но 72 не е просто)
    print(isSpecial 17197 2)   -- → True (числата 17, 71, 19 и 97 са прости)
    print(isSpecial 12234 3)   -- → False (числото 234 не е просто)
    print(isSpecial 10113 3)   -- → True (числата 101, 011 и 113 са прости)
    print(isSpecial 353 2)     -- → False (числото 35 не е просто)

