--problem_01--------------------------------------
checkSequence :: [Int] -> Bool
checkSequence [] = False
checkSequence [_] = True
checkSequence (x1:x2:xs) = if x1 < x2 && x2 `div` x1 /= 0 then checkSequence (x2:xs) else False
--problem_02--------------------------------------
removeNb :: Int -> [(Int, Int)]
removeNb n = [(x,y)|x<-[1..n], y<-[1..n], ((sum [t | t<-[1..n]]) - (x + y)) == x * y]
--problem_03--------------------------------------
type Point = (Double, Double)

myCalc::Point -> Point -> Double
myCalc (x1,y1) (x2, y2) = y1 + (((x2-x1)*(y2-y1))/(x2-x1)))

line :: Point -> Point -> (Double -> Double)
line (x1,y1) (x2, y2) = if x1 /= x2 then myCalc (x1,y1) (x2, y2) else error "x1 == x2"

main :: IO()        
main = do
    --problem_01--tests
    --print(checkSequence [2, 9, 15]) -- True
    --print(checkSequence [11, 14, 20, 27, 31]) --True
    --print(checkSequence [11, 14, 28, 27, 31]) -- False
    --print(checkSequence [11, 14, 14, 29, 31]) -- False

    --problem_02--tests
    --print(removeNb 26)  -- [(15,21),(21,15)]
    --print(removeNb 100) -- []
    --print(removeNb 101) -- [(55,91),(91,55)]

    --problem_03--tests
    print(line (0,0) (1,1))

