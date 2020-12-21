--problem01--------------------------------------------------------

-- Задача 1. Нека f е функция от тип Int -> Int, а lst е списък от цели числа [a1,a2, … ,an].
-- Дефинирайте функция func f lst, която връща функция от тип Int -> Int, чиято стойност за всяко
-- цяло число x е равна на f(a1x) + 2.f(a2x) + … + n.f(anx).

func :: (Int -> Int) -> [Int] -> (Int -> Int)
func f lst = (\x -> ((makeFunk lst f x)))

makeFunk :: [Int]->(Int->Int)->Int->Int
makeFunk (y:ys) f x = helper (y:ys) f x 1
    where 
        helper (y:ys) f x counter
            | [] == ys  = counter * f(y * x)
            | otherwise = counter * f(y * x) + helper ys f x (counter + 1)
--problem02--------------------------------------------------------

-- Задача 2. Нека f е функция от тип Int -> Int, а lst е списък от цели числа [a1,a2, … ,an].
-- Дефинирайте функция func f lst, която връща функция от тип Int -> Int, чиято стойност за всяко
-- цяло число x е равна на a1f(x) + a2f(x2) + … + anf(xn). 

func' :: (Int -> Int) -> [Int] -> (Int -> Int)
func' f lst = (\x -> ((makeFunk' lst f x)))

makeFunk' :: [Int]->(Int->Int)->Int->Int
makeFunk' (y:ys) f x = helper (y:ys) f x 1
    where 
        helper (y:ys) f x counter
            | [] == ys  = y * (f(x ^ counter))
            | otherwise = y * (f(x ^ counter)) + helper ys f x (counter + 1)
--problem03--------------------------------------------------------

-- Задача 3. Дефинирайте функция от по-висок ред boundUp f y, която приема едноаргументна
-- числова функция f и число y и връща като резултат едноаргументна числова функция, чиято
-- стойност във всяка дадена точка x е равна на стойността на f в x, ако тази стойност е по-голяма
-- от y, или на y в противен случай.
--problem04--------------------------------------------------------

-- Задача 4. Разглеждаме непразен списък от едноаргументни функции [f1, f2, … , fn], всяка от
-- които е от тип Int -> Int. Дефинирайте функция getOddCompositionValue, която при подаден
-- такъв списък lst връща като резултат функция, чиято стойност за всяко цяло число x е равна на
-- стойността на композицията на функциите с нечетни поредни номера от lst в x, т.е. на
-- f1(f3( … (x) … )).

-- Пример:
-- getOddCompositionValue [(\x -> x+1),(\x -> x*2),(\x -> x-1)(\x -> x ‘div’ 2)] 2 → 2

--example01
getOddCompositionValue:: [(Int->Int)] -> (Int -> Int)
getOddCompositionValue []     = id
getOddCompositionValue (x:xs) = helper (x:xs) 1
    where
        helper (x:xs) counter
            | null xs       = if (odd counter) then (\y -> x y) else id
            | odd counter   = (\y -> x (helper xs (counter + 1) y))
            | otherwise     = helper xs (counter + 1)
--example02
getOddFunc :: [(Int->Int)] -> [(Int->Int)]
getOddFunc [] = []
getOddFunc lst = helper lst 1
    where
        helper l@(x:xs) counter
            | null xs = if(odd counter) then [x] else []
            | null l = []
            | odd counter = x : helper xs (counter+1)
            | otherwise = helper xs (counter+1)

getOddCompositionValue' :: [(Int->Int)] -> (Int->Int)
getOddCompositionValue' [] = id
getOddCompositionValue' lst = (foldr1 (.) newList)
    where
        newList = (getOddFunc lst)

--problem05--------------------------------------------------------

-- Задача 5. Нека са дефинирани типовете:
-- Дефинирайте функция minDepthGreenNode :: Tree -> Int,
-- която намира дълбочината на най-плиткия (най-близкия до корена) връх с цвят Green на дадено двоично дърво от тип Color.

data Color = Red | Green | Blue 
     deriving (Read, Show, Eq)

data Tree = Empty | Node Color Tree Tree

minDepthGreenNode :: Tree -> Int
minDepthGreenNode tree = helper tree 1
    where
        helper Empty currDepth = currDepth
        helper (Node v lt rt) currDepth
            | v == Green = minimum [currDepth, helper lt(currDepth+1), helper rt (currDepth+1)]
            | otherwise = min (helper lt (currDepth + 1)) (helper rt (currDepth + 1))

t1 :: Tree                                                     
t1 =  Node Blue (Node Blue (Node Green Empty Empty)                         --Blue
            (Node Blue Empty Empty))                          --Blue                    --Red
            (Node Red Empty                              --Green   --Blue                       --Red
            (Node Red Empty Empty))

--problem06--------------------------------------------------------

-- Дефинирайте функция maxDepthBlueNode :: Tree -> Int,
-- която намира дълбочината на най-дълбокия (най-отдалечения от корена) връх с цвят Blue на дадено двоично дърво от тип Color.

maxDepthBlueNode :: Tree -> Int
maxDepthBlueNode tree = helper tree 1
    where
        helper Empty _ = 0
        helper (Node v lt rt) currDepth
            | v == Blue = maximum [currDepth, helper lt (currDepth+1), helper rt (currDepth+1)]
            | otherwise = max (helper lt (currDepth + 1)) (helper rt (currDepth + 1))

--problem07--------------------------------------------------------
type Name = String -- име
type Capital = Name -- столица
type AvgYearlyTemperature = Double -- средногодишна температура
type Elevation = Int -- надморска височина
data City = City Name Elevation AvgYearlyTemperature -- град
    deriving (Read, Show)
data Country= Country Name Capital [City] -- държава
    deriving (Read, Show)

-- Дефинирайте функция coldestCapital :: [Country] -> Name, която получава като аргумент списък
-- от държави и връща като резултат името на държавата от списъка с най-студена столица
-- (столица с най-ниска средногодишна температура).

coldestCapital :: [Country] -> Name
coldestCapital counties = getColdest(getCountries counties)

getColdestCity :: [City] -> String-> (Capital,AvgYearlyTemperature)
getColdestCity cs cap =  minimum [(na,av) |  City na el av  <- cs, cap == na]

getCountries :: [Country] -> [(String,Double)]
getCountries countr = [ getColdestCity citis cap |  Country na cap citis  <- countr]

getColdest :: [(String,Double)] -> String
getColdest [] = ""
getColdest [(name,temp)] = name
getColdest (c1@(name1, temp1):c2@(name2, temp2):cs) = if temp1 > temp2 then getColdest (c2:cs) else getColdest (c1:cs)

c1 :: City
c1 = (City "Sofia" 3 5.5)

c3 :: City
c3 = (City "Sofia1" 3 2.5)

c2 :: City
c2 = (City "Skopie" 3 (-3.5))

cit :: [City]
cit = [c1,c2,c3]

countr ::Country
countr = Country "Bul" "Sofia" cit

countr1 ::Country
countr1 = Country "Makedonja" "Skopie" cit

countries :: [Country]
countries = [countr,countr1]

main :: IO()
main = do
    --problem01-TEST----------
    print(func (+1) [1,2,3] 2)
    --problem02-TEST----------
    print(func' (+1) [1,2,3] 2)
    --problem03-TEST----------
    --unsolved--
    --problem04-TEST----------
    print(getOddCompositionValue [(\x -> x+1), (\x -> x*2), (\x -> (x-1)), (\x -> x `div` 2)] 2) -- --> 2
    print(getOddCompositionValue' [(\x -> x+1), (\x -> x*2), (\x -> (x-1)), (\x -> x `div` 2)] 2) -- --> 2
    --problem05-TEST----------
    print(minDepthGreenNode t1)
    --problem06-TEST----------
    print(maxDepthBlueNode t1)
    --problem07-TEST----------
    print(coldestCapital countries)