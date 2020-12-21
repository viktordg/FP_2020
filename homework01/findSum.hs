findSum1 :: Int -> Int -> Int -> Int
findSum1 a b n = helper a b n 0 0
    where
        helper a b n p r
            | p <= n - 2    = r + (b * (2^p)) + helper a b n (p + 1) r
            | p == n - 1    = r + a +(b * (2^p)) + helper a b n (p + 1) r
            | otherwise     = r

findSum2 :: Int -> Int -> Int -> Int
findSum2 a b n = helper a b n 0 0
    where
        helper a b n p r
            | p <= n - 3     = r + (b * (2^p)) + helper a b n (p + 1) r
            | p == n - 2    = r + a +(b * (2^p)) + helper a b n (p + 1) r
            | otherwise     = r

findSum3 :: Int -> Int -> Int -> Int
findSum3 a b n = helper a b n 0 0
    where
        helper a b n p r
            | p <= n - 4     = r + (b * (2^p)) + helper a b n (p + 1) r
            | p == n - 3    = r + a +(b * (2^p)) + helper a b n (p + 1) r
            | otherwise     = r

findSum :: Int -> Int -> Int -> Int
findSum a b c = a + b + c

main :: IO()
main = do
    print(findSum (findSum1 0 2 10) (findSum2 0 2 10) (findSum3 0 2 10)) 
    print(findSum (findSum1 5 3 5) (findSum2 5 3 5) (findSum3 5 3 5)) 
