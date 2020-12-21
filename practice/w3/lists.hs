---null
null' :: [a] -> Bool
null' [] = True
null' _  = False
---head
head' :: [a] -> a
head' (x:_) = x
---tail
tail' :: [a] -> [a]
tail' (_:xs) = xs
---sum
sum' :: [Int] -> Int
sum' []     = 0
sum' (x:xs) = x + sum' xs
---lenght
lenght' :: [a] -> Int
lenght' []     = 0
lenght' (_:xs) = 1 + lenght' xs
---element
elem' :: Int -> [Int] -> Bool
elem' _ []     = False
elem' n (x:xs) = n == x || elem' n xs
---take
take' :: Int -> [Int] -> [Int]
take' 0 _       = []
take' _ []      = []
take' n (x:xs)  = x : take' (n - 1) xs
---drop
drop' :: Int -> [a] -> [a]
drop' 0 xs      = xs
drop' _ []      = []
drop' n (_:xs)  = drop' (n-1) xs
---minimum 
minimum' :: [Int] -> Int
minimum' (x:xs) = helper xs x
    where 
        helper [] min       = min
        helper (x:xs) min   = if x < min then helper xs x else helper xs min
main :: IO()    
main = do
    --null
    print (null' [])
    print (null' [1,2,3])
    --head
    print (head' [1,2,3])
    --tail
    print (tail' [1,2,3])
    --sum 
    print (sum' [1,2,3])
    --lenght
    print (lenght' [1])
    --elemet
    print (elem' 5 [1,2,3])
    --take
    print (take' 2 [1,2,3])
    --drop
    print (drop' 2 [1,2,3])
    --minimum
    print (minimum' [2,5,6])