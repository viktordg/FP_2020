--1
addPair :: (Int, Int) -> Int
addPair (x, y) = x + y
--2
divide :: Int -> Int -> (Int, Int)
divide x y = (x `div` y, x `mod` y)
--3
type Vector = (Double, Double, Double)
v = (1, 2, 3)
p = (4, 5, 6)
---sumVectors
sumVectors :: Vector -> Vector -> Vector
sumVectors (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
---scaleVectors
scaleVectors :: Vector -> Double -> Vector
scaleVectors (x, y, z) s = (x * s, y * s, z * s)
---dotProduct 
dotProduct :: Vector -> Vector -> Double
dotProduct (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2
---crossProduct
crossProduct :: Vector -> Vector -> Vector
crossProduct (x1, y1, z1) (x2, y2, z2) = (y1 * z2 - z1 * y2, z1 * x2 - x1 * z2, x1 * y2 - y1 * x2)
---magnitude
magnitude :: Vector -> Double
magnitude (x, y, z) = sqrt (x * x + y * y + z * z)
--4
type Rat = (Int, Int)
r1 = (5, 6)
r2 = (8, 9)
---sumRat
sumRat :: Rat -> Rat -> Rat
sumRat (a, b) (c, d) = (a * d + b * c, b * d)
---multipyRat
multiplyRat :: Rat -> Rat -> Rat
multiplyRat (a, b) (c, d) = (a * c, b * d)
---divideRat
divideRat :: Rat -> Rat -> Rat
divideRat (a, b) (c, d) =  multiplyRat (a,b) (d,c)
---equalRat
equalRat :: Rat -> Rat -> Bool
equalRat (a, b) (c, d) = a * d == b * c
---normalizeRat
normalizeRat :: Rat -> Rat
normalizeRat (a, b) = (a `div` d, b `div` d)
    where d = gcd a b
---sumRatNorm
sumRatNorm :: Rat -> Rat -> Rat
sumRatNorm x y = normalizeRat(sumRat x y)
main :: IO()  
main = do
    --1
    print (addPair (5, 6))
    --2
    print (divide 5 9)
    --3
    print (sumVectors v p)
    print (scaleVectors v 5)
    print (dotProduct v p)
    print (crossProduct v p)
    print (magnitude v)
    --4
    print (sumRat r1 r2)
    print (multiplyRat r1 r2)
    print (divideRat r1 r2)
    print (equalRat r1 r2)
    print (normalizeRat r2)
    print (sumRatNorm r1 r2)