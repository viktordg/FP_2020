isAscending :: Int -> Bool 
isAscending n 
    | n < 10                                            = True
    | n >= 10 && (n `mod` 10) <= ((n `div` 10) `mod`10) = False
    | n >= 10 && (n `mod` 10) >= ((n `div` 10) `mod`10) = isAscending (n `div` 10)
   

main :: IO()
main = do   
    print(isAscending 10)