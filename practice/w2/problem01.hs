countDigits :: Int -> Int 
countDigits n
    |n<10       = 1
    |otherwise  = 1 + countDigits(n `div` 10)

main :: IO()
main = do
    print(countDigits 56)    