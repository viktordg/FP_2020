findSum :: Int -> Int -> Int -> Int
findSum a b n = helper a b n 0 0 
    where
        helper a b n p r
            | n < 3         = error "n must be >3"
            | p <= n - 2    = r + (b * (2^p)) + helper a b n (p + 1) r
            | p == n - 1    = r + a + (b * (2^p)) + helper a b n (p + 1) r + (r - (b * (2^(p-1)))) - (r - (2 * (b * (2^p)))) + 4
            | otherwise     = r
main :: IO()
main = do
    print(findSum 0 2 10)
    print(findSum 5 3 5) 
    print(findSum 1 2 2)

