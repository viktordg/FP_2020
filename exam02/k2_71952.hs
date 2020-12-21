-----problem01---------------------------------------------------
rotate :: Int -> [a] -> [a]
rotate n xs
    | n >= 0    = drop n xs ++ take n xs         
    | n < 0     = drop len xs ++ take len xs
        where len = n + length xs    
-----problem02---------------------------------------------------
data BTree = Empty | Node Int BTree BTree

mirror:: BTree -> BTree -> Bool
mirror Empty          Empty             = True
mirror (Node v1 lt1 rt1) (Node v2 lt2 rt2)      = v1 == v2 && mirror lt1 rt2 && mirror rt1 lt2
mirror _              _                 = False

isSymmetric :: BTree -> Bool
isSymmetric Empty = True
isSymmetric (Node _ lt rt) = mirror lt rt

t3 :: BTree
t3 = Node 1 (Node 2 Empty Empty) 
            (Node 3 Empty Empty) 

t4 :: BTree
t4 = Node 1 (Node 2 (Node 3 Empty Empty) 
            Empty)
            (Node 2 Empty
            (Node 3 Empty Empty)) 

t5 :: BTree
t5 = Node 1 (Node 2 (Node 3 Empty Empty)
            (Node 7 (Node 5 Empty Empty) 
            Empty)) 
            (Node 2 (Node 7 Empty 
            (Node 5 Empty Empty)) 
            (Node 3 Empty Empty))

-----problem03--------------------------------------------------- 
data NestedList = Elem Int | List [NestedList]
flatten :: NestedList -> [Int]
flatten (List [])       = []
flatten (Elem a)        = [a]
flatten (List (x:xs))   = flatten x ++ flatten (List xs)

main :: IO()
main = do
    --problem01--Test--
    print(rotate 3 ['a','b','c','d','e','f','g','h'] )                          --  "defghabc"
    print(rotate (-2) ['a','b','c','d','e','f','g','h'])                        --  "ghabcdef"
    --problem02--Test--
    print(isSymmetric t3)                                                       -- False
    print(isSymmetric t4)                                                       -- True
    print(isSymmetric t5)                                                       -- True
     --problem03--Test--
    print(flatten(List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])) -- [1,2,3,4,5]
    print(flatten (Elem 1))                                                     -- [1]