isPrime :: Int -> Bool
isPrime n = helper (n - 1)
    where
        helper current
            | current == 1                  = True
            | (n `mod` current) == 0        = False
            | otherwise                     = helper(current - 1)



main :: IO()
main = do
    print (isPrime 31)