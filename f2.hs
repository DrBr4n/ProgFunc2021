import Data.Char

--1

--a)
-- 2^2 + 3^2 + 5^2 + 1^2 + 0
-- 4 + 9 + 25 + 1 + 0
-- 39

--b)
-- [8,12]

--c)
-- funC[3,4,5]
-- funC[5]
-- []

--d)
--funD "otrec" = g [] "otrec"
-- g "otrec" [] = l
-- g "otrec" (h:t) = g (h:"otrec") t

--2
--a)
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = h*2 : dobros t

--b)
numOcorre :: Char -> String -> Int
numOcorre x [] = 0
numOcorre x (l:lt) | x == l = (numOcorre x lt) + 1
                   | x /= l = numOcorre x lt 

--c)
positivos :: [Int] -> Bool 
positivos [] = True
positivos (h:t) | h >= 0 = positivos t
                | otherwise = False 

--d)
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) | h >= 0 = h : soPos t
            | otherwise = soPos t

--e)
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) | h < 0 = h + somaNeg t
              | otherwise = somaNeg t 

--f)
tresUlt :: [a] -> [a]
tresUlt l | length l <= 3 = l
          | otherwise = tresUlt (tail l)

--g)
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((a,b):t) = b : segundos t

--h)
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros x [] = False
nosPrimeiros x ((a,b):t) | x == a = True 
                         | otherwise = nosPrimeiros x t

--i)
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((a,b,c):t) = (a + aa r, b + bb r, c + cc r)
    where r = sumTriplos t

aa :: (a,b,c) -> a
aa (a,b,c) = a

bb ::(a,b,c) -> b
bb (a,b,c) = b

cc ::(a,b,c) -> c
cc (a,b,c) = c

--3
--a)
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) | isDigit h = h : soDigitos t
                | otherwise = soDigitos t

--b)
minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t) | isLower h = 1 + minusculas t
                 | otherwise = minusculas t

--c)
--nums :: String -> [Int]

--4)

type Polinomio = [Monomio]

type Monomio = (Float,Int)

--a)
conta :: Int -> Polinomio -> Int
conta n [] = 0
conta n (h:t) | n == snd h = 1 + conta n t
              | otherwise = conta n t

--b)
grau :: Polinomio -> Int
grau [] = 0
grau (h:t) = max (snd h) (grau t)

--c)
selgrau :: Int -> Polinomio -> Polinomio
selgrau n [] = []
selgrau n (h:t) | n == snd h = h : selgrau n t
                | otherwise = selgrau n t

--d)
--deriv :: Polinomio -> Polinomio
--deriv [] = []
--deriv ((c,e):t) = (e * c, e - 1)

--e)
calcula :: Float -> Polinomio -> Float 
calcula n [] = 0
calcula n ((c,e):t) = c * n^e + calcula n t

--f)
simp :: Polinomio -> Polinomio
simp [] = []
simp (h:t) | fst h == 0 = simp t
           | otherwise = h : simp t

--g)
--mult :: Monomio -> Polinomio -> Polinomio

--h)
--normaliza :: Polinomio -> Polinomio 

--i)
--soma :: Polinomio -> Polinomio -> Polinomio

--j)
--produto :: Polinomio -> Polinomio -> Polinomio

--k)
--ordena :: Polinomio -> Polinomio

--l)
equiv :: Polinomio -> Polinomio -> Bool
equiv a b | calcula 2 a == calcula 2 b = True 
          | otherwise = False 