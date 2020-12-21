isPerfect :: Int -> Bool 
isPerfect n = helper (n `div` 2) 0
    where 
        helper current sumOfD
            | current == 0 && sumOfD == n           = True
            | current == 0 && sumOfD /= n           = False
            | otherwise                             = helper (current - 1) (sumOfD + current)

main :: IO()
main = do
    print(isPerfect 28)