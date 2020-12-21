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
        

main :: IO()
main = do
    print(isSpecial 131 2)     -- → True (числата 13 и 31 са прости)
    print(isSpecial 472 2)     -- → False (47 е просто число, но 72 не е просто)
    print(isSpecial 17197 2)   -- → True (числата 17, 71, 19 и 97 са прости)
    print(isSpecial 12234 3)   -- → False (числото 234 не е просто)
    print(isSpecial 10113 3)   -- → True (числата 101, 011 и 113 са прости)
    print(isSpecial 353 2)     -- → False (числото 35 не е просто)