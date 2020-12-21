isPalindrome :: Int -> Bool
isPalindrome n = helper n n 0
    where 
        helper original n reversed
            | n == 0 && original == (reversed * 10)             = True
            | n == 0 && original /= (reversed * 10)             = False
            | n < 10 && original == ((reversed * 10) + n )      = True          
            | n < 10 && original /= ((reversed * 10) + n )      = False
            | otherwise                                         = helper original (n `div` 10) ((reversed * 10) + (n `mod` 10))



main :: IO()
main  = do 
    print(isPalindrome 1111)