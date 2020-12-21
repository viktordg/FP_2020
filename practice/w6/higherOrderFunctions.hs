---01.1
incrementAllBy :: [Int] -> Int -> [Int]
incrementAllBy xs n = map(\ x -> x+n) xs
---01.2
filterSmallerThan :: [Int] -> Int -> [Int]
filterSmallerThan xs n = filter (\x -> x<n) xs
---01.3
multiplyAllBy :: [Int] -> Int -> [Int]
multiplyAllBy xs n = map (*n) xs
---02
splitByParity :: [Int] -> ([Int], [Int])
splitByParity xs = (filter odd xs, filter even xs)
---03
partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' f [] = ([], [])
partition' f xs = (filter f xs, filter(not.f) xs)
---04
splitByParity' :: [Int] -> ([Int], [Int])
splitByParity' xs   = partition' odd xs
---05
quickSort :: [Int] -> [Int]
quickSort []     = []
quickSort (p:xs) = quickSort smaller ++ [p] ++ quickSort larger
    where (smaller, larger) = partition' (<p) xs
---06
-- Нека as = [a1, a2 … , ak] и bs = [b1, b2 … , bk] са непразни списъци с еднакъв брой числа.
-- Да се дефинира предикат isImage :: [Int] -> [Int] -> Bool, 
-- който да връща „истина“ точно когато съществува такова число x, че ai = x + bi за всяко i = 1,..., k.

---07
-- Да се дефинира предикат isTriangular :: [[Int]] -> Bool, който получава квадратна числова матрица, представена като списък от списъци, 
-- и проверява дали тя е горно триъгълна, т.е. дали всичките елементи под главния ѝ диагонал са нули.

main :: IO()
main = do
    print(incrementAllBy [1,2,3] 2)
    print(filterSmallerThan [1,2,3] 2)
    print(multiplyAllBy [1,2,3] 2)
    print(splitByParity [1,2,3])
    print(partition' (<5) [1,2,3])
    print(splitByParity' [1..10])
    print(quickSort [1,5,894648,10])