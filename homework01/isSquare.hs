isSquare :: Int -> Bool
isSquare n = helper n 1
    where 
        helper n i
            | i * i == n                        = True
            | i * i /= n && i > (n `div` 2)     = False
            | otherwise                         = helper n (i + 1)

main :: IO()
main = do
    print(isSquare 1)
    print(isSquare 2)
    print(isSquare 4)
    print(isSquare 17)
    print(isSquare 256)
    print(isSquare 2500)
