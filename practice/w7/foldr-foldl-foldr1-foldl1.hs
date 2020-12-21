main :: IO()
main = do
    print(foldr (\x y -> (x+y)/2) 54 [12, 4, 10, 6])
    print(foldr (\x y -> concat ["(",x,"+",y,")"]) "0" ["first","second", "third"])