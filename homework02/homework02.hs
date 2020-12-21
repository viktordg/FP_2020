---------------TASK-01---------------
generate::Double->Double->[Double]
generate p n = helper p n 1 1.0
    where
        helper p n m s
            | m <= n    = s : helper p n (m + 1) (s + (1 / ((m+1) ** p)))
            | otherwise = []
---------------TASK-02---------------
isSquare :: Int -> Bool
isSquare n = helper n 1
    where 
        helper n i
            | i * i == n                        = True
            | i * i /= n && i > (n `div` 2)     = False
            | otherwise                         = helper n (i + 1)

sumSquaredDevidors :: Int -> Int
sumSquaredDevidors n = helper n 1 0
    where   
        helper n i s
            | n >= i && (n `mod` i == 0)  = helper n (i+1) (s+(i*i))
            | n >= i && (n `mod` i /= 0)  = helper n (i+1) s
            | otherwise                   = s

listSquares :: Int->Int->[(Int,Int)]
listSquares a b = helper a b a 
    where
        helper a b i
            | a <= i && i <= b && (isSquare i) == True      = (i,(sumSquaredDevidors i)) : helper a b (i+1)
            | a <= i && i <= b && (isSquare i) == False     = helper a b (i+1)
            | otherwise                                     = []

---------------TASK-03---------------
type Point = (Double, Double)

checkIsInside :: Point->Double->Point->Bool
checkIsInside (ax,ay) r (bx,by) = if (sqrt(((bx-ax)**2) + ((by-ay)**2)) <= r) then True else  False

insideList :: Point -> Double -> [Point] -> [Point]
insideList p r []       = []
insideList p r (q:qs)
    | checkIsInside p r q == True    = q : insideList p r qs
    | checkIsInside p r q == False   = insideList p r qs

outsideList :: Point -> Double -> [Point] -> [Point]
outsideList p r []      = []
outsideList p r (q:qs)
    | checkIsInside p r q == False  = q : outsideList p r qs
    | checkIsInside p r q == True   = outsideList p r qs

splitPoints :: Point -> Double -> [Point] -> ([Point], [Point])
splitPoints p r qs = (insideList p r qs, outsideList p r qs)
---------------TASK-04---------------
type Account = (Int, Int, Double)
type Person = (Int, String, String)
 
accCount :: [Account] -> Int-> Int
accCount (a@(_,personId,_):as) id
    | ((as == []) && (id == personId))  = 1
    | as == []                          = 0
    | personId == id                    = 1 + (accCount as id)
    | otherwise                         = 0 + (accCount as id)
 
getBalance :: [Account] -> Int -> Double
getBalance [] _                                         = 0
getBalance (a@(_, personId, balance):as) inputId    = if personId == inputId then balance + getBalance as inputId else getBalance as inputId
 
 
getAverageBalance :: ([Account], [Person]) -> (Person -> Bool) -> Double
getAverageBalance (_, []) _ = 0
getAverageBalance ([], _) _ = 0
getAverageBalance (accs, p@(accId,personId,city):ps) f = helper (accs, p:ps) f 0.0 0
    where
        helper (accs, p:ps) f a c
            | f p == True && ps == []       = (a + (getBalance accs personId)) / fromIntegral(c + (accCount accs personId))
            | f p == False && ps == []      = a / fromIntegral(c)
            | f p == True                   = helper (accs, ps) f (a + (getBalance accs personId)) (c + (accCount accs personId))
            | f p == False                  = helper (accs, ps) f a c  

ps :: [Person]
ps = [(1, "Ivan", "Sofia"), (2, "Georgi", "Burgas"), (3, "Petar", "Plovdiv"), (4, "Petya", "Burgas")]

as :: [Account]
as = [(1, 1, 12.5), (2, 1, 123.2), (3, 2, 13.0), (4, 2, 50.2), (5, 2, 17.2), (6, 3, 18.3), (7, 4, 19.4)]

main :: IO()        
main = do
    --TASK-01---------------------------
    print(generate 1 3)             --result-->[1.0,1.5,1.8333333333333333]                                      
    print(generate 0.1 5)           --result-->[1.0,1.93,2.83,3.70,4.55]
    --TASK-02---------------------------
    print(listSquares 1 30)         --result-->[(1,1),(4,21),(9,91),(16,341),(25,651)]
    print(listSquares 250 300)      --result-->[(256,87381),(289,83811)]
    --TASK-02---------------------------
    print(splitPoints (1,1) 5 [(1,2),(2,3),(10,15),(-1,1),(12,14)]) 
    --result-->([(1.0,2.0),(2.0,3.0),(-1.0,1.0)],[(10.0,15.0),(12.0,14.0)])
    print(splitPoints (10,10) 5 [(1,2),(2,3),(10,15),(-1,1),(12,14)]) 
    --result-->([(10.0,15.0),(12.0,14.0)],[(1.0,2.0),(2.0,3.0),(-1.0,1.0)])
    --TASK-04---------------------------  
    print(getAverageBalance (as,ps) (\ (_,_,city) -> city == "Burgas")) --result-->24.95
    print(getAverageBalance (as,ps) (\ (_,(n:_),_) -> n == 'P'))        --result-->18.85
