isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n 
    | (length [x | x <- [2 .. (n `div` 2)], n `mod` x == 0]) > 0    = False
    | otherwise                                                     = True

reverseNumber :: Integer -> Integer
reverseNumber n = helper n 0
    where
        helper n result
            | n == 0                = (result * 10)
            | n < 10                = (result * 10) + n
            | otherwise             = helper (n `div` 10) ((result * 10) + (n `mod` 10))

isSpecial :: Integer -> Integer -> Bool
isSpecial n k = helper n k n 0 0
    where 
        helper n k t r c 
            | n < 10                                                    = error "n must be >= 10"
            | t >= 10 && c < k                                          = helper n k (n `div` 10) ((r + (t `mod` 10)) * 10) (c + 1)
            | c == k && isPrime(reverseNumber(r `div` 10))  == True     = helper n k t (((reverseNumber(r `div` 10)) `div` 10) * 10) (c - 1)
            | c == k && isPrime(reverseNumber(r `div` 10))  == False    = False
            | t == 0 && isPrime(reverseNumber(r `div` 10))  == True     = True
            | otherwise                                                 = False


main :: IO()
main = do
    print(isSpecial 131 2)     -- → True (числата 13 и 31 са прости)
    print(isSpecial 472 2)     -- → False (47 е просто число, но 72 не е просто)
    print(isSpecial 17197 2)   -- → True (числата 17, 71, 19 и 97 са прости)
    print(isSpecial 12234 3)   -- → False (числото 234 не е просто)
    print(isSpecial 10113 3)   -- → True (числата 101, 011 и 113 са прости)
    print(isSpecial 353 2)     -- → False (числото 35 не е просто)

    print((97 `div` 10) * 10)
    print( 8 `div` 10)