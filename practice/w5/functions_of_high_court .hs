--example
addNum :: Int -> (Int -> Int)
addNum x = addX
    where addX y = x + y

addNum' :: Int -> (Int -> Int)
addNum' x = \ y -> x + y

--Задача 1. Да се дефинира функция inside :: Int -> Int -> (Int ->
--Bool), която получава само 2 аргумента - двата края на интервал. inside a
--b връща функция на 1 аргумент, която проверява дали той е в интервала
--[a,b].
inside :: Int-> Int -> (Int -> Bool)
inside a b = \ x ->  a <= x && x <= b

--Задача 2. Да се дефинира функция twice f, която получава едноаргументна
--функция f и я прилага два пъти.
twice :: (a -> a) -> (a -> a)
twice f = f . f

--Задача 3. Да се дефинира функция iter n f, която получава едноаргументна
--функция f и я прилага n пъти.
iter :: Int -> (a -> a) -> (a -> a)
iter 0 _ = id 
iter n f = f . (iter (n-1) f) 

--Задача 4. Да се дефинира функция xSquaredPlusOne :: Int -> Int, която
--пресмята x^2 + 1, ползвайки композиция на функции.
xSquaredPlusOne :: Int -> Int
xSquaredPlusOne x = ((\ x -> x + 1) . (\ x -> x * x)) x

--Задача 5. Да се дефинира функция xPlusOneSquared :: Int -> Int, която
--пресмята (x+1)^2, ползвайки композиция на функции.
xPlusOneSquared :: Int -> Int
xPlusOneSquared x = ((\ x -> x * x) . (\ x -> x + 1)) x

--------------------------Частично прилагане--------------------------



main :: IO()
main = do
    --example
    print((addNum 5) 4)
    print(addNum 5 4)
    print(addNum' 5 4)
    --Задача 1.
    print(inside 3 5 4)
    --Задача 2.
    print(twice (\ x-> x * x) 3)
    --Задача 3.
    print(iter 5 (\ x -> x * x) 3)
    --Задача 4.
    print(xSquaredPlusOne 2)
    --Задача 5.
    print(xPlusOneSquared 2)