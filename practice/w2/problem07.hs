reverseNumber :: Int -> Int
reverseNumber n = helper n 0
    where
        helper n result
            | n == 0                = (result * 10)
            | n < 10                = (result * 10) + n
            | otherwise             = helper (n `div` 10) ((result * 10) + (n `mod` 10))


main :: IO()
main = do
    print(reverseNumber 456)