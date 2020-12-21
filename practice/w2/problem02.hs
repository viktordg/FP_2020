
sumDigits :: Int -> Int 
sumDigits 0 = 0
sumDigits n = (n `mod` 10) + sumDigits(n `div` 10)

main :: IO()
main = do
    print(sumDigits 52)