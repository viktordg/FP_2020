import Data.List(inits, tails) -- само тези двете функции

--Задача 1. Да се дефинира функция removeFirst x xs, която изтрива
--първото срещане на елемента x в списъка xs.
removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ []                = []
removeFirst x (y:ys)            = if x == y then ys else y : removeFirst x ys

--Задача 2. Да се дефинира функция removeAll x xs, която изтрива всички
--срещания на даден елемент на списък.
removeAll :: Eq a => a -> [a] -> [a]
removeAll _ []                  = []
removeAll x xs                  = [y | y <- xs , y /= x]

removeAll' :: Eq a => a -> [a] -> [a]
removeAll' _ []                 = []
removeAll' x (y:ys)             = if x == y then removeAll' x ys else y : removeAll' x ys

--Задача 3. Да се дефинира функция removeDuplicates xs, която премахва
--всички повторни срещания на елементи от списъка xs
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates []             = []
removeDuplicates (x:xs)         = x : removeDuplicates  (removeAll x xs)

----removeDuplicates'' :: Eq a => [a] -> [a]
----removeDuplicates'' []           = []
----removeDuplicates'' [x]          = [x]
----removeDuplicates''

--Задача 4. Да се дефинира функция prefix xs ys, която проверява дали xs е
--префикс на ys.
prefix :: Eq a => [a] -> [a] -> Bool
prefix [] _                     = True
prefix xs []                    = False
prefix (x:xs) (y:ys)            = if (x /= y) then False else (prefix xs ys)

prefix' :: Eq a => [a] -> [a] -> Bool
prefix' [] _                    = True
prefix' _ []                    = False
prefix' (x:xs) (y:ys)           = x == y && prefix' xs ys

prefix'' :: Eq a => [a] -> [a] -> Bool
prefix'' xs ys                  = xs == (take (length xs) ys)

prefix4 :: Eq a => [a] -> [a] -> Bool
prefix4 xs ys                   = xs `elem` (inits ys)

--Задача 5. Да се дефинира функция countOccurrences subxs xs, която
--връща броя срещания на подсписъка subxs в xs.
countOccurences :: Eq a => [a] -> [a] -> Int
countOccurences _ [] = 0
countOccurences subxs list@(x:x1) = countOccurences subxs x1 + count
    where
        count = if prefix subxs list then 1 else 0

main :: IO()
main = do
    --Задача 1.
    print(removeFirst 2 [1,2,3,5,2,6])
    --Задача 2.
    print(removeAll' 2 [1,2,3,5,2,6])
    print(removeAll  2 [1,2,3,5,2,6])
    --Задача 3.
    print(removeDuplicates [1,2,3,5,2,6])
    ----print(removeDuplicates'' [1,2,3,5,2,6])
    --Задача 4.
    print(prefix [1,2,3,6] [1,2,3,6,9,8,7,8])
    print(prefix' [1,2,3,6] [1,2,3,6,9,8,7,8])
    print(prefix'' [1,2,3,6] [1,2,3,6,9,8,7,8])
    print(prefix4 [1,2,3,6] [1,2,3,6,9,8,7,8])
    --Задача 5.
    print(countOccurences [1,2,3,6] [1,2,3,6,9,8,7,8])