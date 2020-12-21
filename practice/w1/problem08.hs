import System.IO

myFib :: Int -> Int
myFib 0 = 1
myFib 1 = 1
myFib n = myFib (n-1) + myFib(n-2)

b :: Int 
b=6

main :: IO()
main = do
    print(myFib 5)
