--1
--a)
perimetro :: Float -> Float
perimetro n = 2 * pi * n

--b)
type Ponto = (Double, Double)

dist :: Ponto -> Ponto -> Double
dist (x,y) (a,b) = sqrt k
    where k = (a-x)^2 + (b-y)^2

--c)
primUlt :: [a] -> (a,a)
primUlt l = (head l, last l)

--d)
multiplo :: Int -> Int -> Bool
multiplo m n | mod m n == 0 = True
             | otherwise = False

--e) 
truncaImpar :: [a] -> [a]
truncaImpar l | mod (length l) 2 == 0 = l
              | otherwise = tail l

--f)
max2 :: Int -> Int -> Int
max2 a b | a >= b = a
         | otherwise = b

--g)
max3 :: Int -> Int -> Int -> Int
max3 a b c = max2 (max2 a b) c

--2
--a)
nRaizes :: Int -> Int -> Int -> Int
nRaizes a b c | bin < 0 = 0
              | bin == 0 = 1
              | bin > 0 = 2
    where bin = b^2 - 4 * a * c

--b)

--3

type Hora = (Int, Int)

--a)
valida :: Hora -> Bool
valida (h,m) | hvalid && mvalid = True
             | otherwise = False
    where 
        hvalid = h >= 0 && h < 24
        mvalid = m >= 0 && m < 60

--b)
depois :: Hora -> Hora -> Bool
depois (h,m) (hs,ms) | h == hs && m > ms = True
                     | h > hs = True
                     | otherwise = False

--c) 
converte :: Hora -> Int
converte (h,m) = h * 60 + m

--d)
converteM :: Int -> Hora
converteM m = 