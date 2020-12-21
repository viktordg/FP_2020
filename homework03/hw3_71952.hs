data BTree a = Empty | Node a (BTree a) (BTree a)
data NTree = Nil | NNode Int [NTree]
---problem_01--------------------------------------------------------------
myContains :: String -> [String] -> Bool
myContains word []      = False
myContains "" [_]       = False
myContains word (x:xs)  = if word == x then True else myContains word xs

containsWord :: BTree Char -> String -> Bool
containsWord tree word = myContains word (genWords tree) 
---problem_02--------------------------------------------------------------
treeLeaves :: BTree Char -> [String]
treeLeaves Empty                = []
treeLeaves (Node v Empty Empty) = [[v]]
treeLeaves (Node _ lt rt)       = treeLeaves lt ++ treeLeaves rt

rootToLeaves :: BTree Char -> [String]
rootToLeaves Empty                  = []
rootToLeaves (Node v Empty Empty)   = [[v]]
rootToLeaves (Node v lt rt) = map (v:) (rootToLeaves lt ++ rootToLeaves rt)

allRootToLeaves :: BTree Char -> [String]
allRootToLeaves Empty                  = []
allRootToLeaves (Node v Empty Empty)   = [[v]]
allRootToLeaves tree@(Node v lt rt)    = rootToLeaves tree ++ rootToLeaves lt ++ rootToLeaves rt

combine:: [String] -> [String] -> [String]
combine [] [] = []
combine [] ys = ys
combine xs [] = xs
combine xs ys = xs ++ ys

genWords :: BTree Char -> [String]
genWords Empty = []
genWords tree@(Node v lt rt) = combine (allRootToLeaves tree) (treeLeaves tree)
---problem_03--------------------------------------------------------------
myContains' :: [String] -> [String] -> [String]
myContains' [] []       = []
myContains' [] ys       = []
myContains' xs []       = []
myContains' (x:xs) ys   = if myContains x ys then x : myContains' xs ys else myContains' xs ys


allContain :: [BTree Char] -> [String]
allContain []           = []
allContain [x]          = genWords x
allContain (x1:x2:xs)   = myContains' (genWords x1) (genWords x2) ++ allContain xs
---problem_04--------------------------------------------------------------
getRoot :: NTree -> Int
getRoot Nil            = 0
getRoot (NNode v _)    = v

isGraceful :: NTree -> Bool
isGraceful Nil              = False
isGraceful (NNode v [])     = True
isGraceful (NNode v (x:xs)) = if even (abs (v - (getRoot x))) && length xs /= 0 then isGraceful x else False

---Example_Trees-----------------------------------------------------------
t1 :: BTree Char
t1  =   Node 'a'    (Node 'c'(Node 'f' Empty Empty)
                            (Node 'd' Empty Empty)) 
                    (Node 'b' Empty
                            (Node 'e' Empty Empty))

t2 :: BTree Char 
t2 =    Node 'a'    (Node 'c' (Node 'd' Empty Empty) 
                        Empty) 
                    (Node 'b' Empty Empty)

t3 :: NTree
t3 = NNode 1 [NNode 3 [],
            NNode 5 [], 
            NNode 7 [],
            NNode 9 []]

t4 :: NTree
t4 = NNode 7 [NNode 9 [NNode 5 [],
                    NNode 2 []]]
main :: IO()        
main = do
    ---Problem_01_TEST--------------------------------------------------------------------

    print(containsWord t1 "acd")    --True
    print(containsWord t1 "cd")     --True
    print(containsWord t1 "ac")     --False

    ---Problem_02_TEST--------------------------------------------------------------------

    print(genWords t1)              -- ["acf","acd","abe","cf","cd","f","d","be","e"]
    print(genWords t2)              -- ["acd","ad","ab","cd","d","b"]

    ---Problem_03_TEST--------------------------------------------------------------------

    print(allContain [t1,t2])

    ---Problem_04_TEST--------------------------------------------------------------------

    print(isGraceful t3) -- True (|3-1|=2, |5-1|=4, |7-1|=6, |9-1|=8)
    print(isGraceful t4) -- False (|9-7|=2, |5-9|=4, (|2-9|=7) --> False)

