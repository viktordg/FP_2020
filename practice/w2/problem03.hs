countOccurences :: Int -> Int -> Int
countOccurences n a
    | n < 10 && n /= a      = 0
    | n < 10 && n == a      = 0
    | (n `mod` 10) == a     = 1
    | otherwise             = countOccurences (n `div` 10) a


main :: IO()
main = do
    print(countOccurences 5856 1)
    