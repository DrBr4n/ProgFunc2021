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
converteM m =  (div m 60, mod m 60)

--e)
dif :: Hora -> Hora -> Int
dif x y | p == q = 0
        | p > q = p - q
        | p < q = q - p 
    where
        p = converte x
        q = converte y
    
--f)
add :: Int -> Hora -> Hora
add n x = converteM(n + converte x)

--5

data Semaforo = Verde | Amarelo | Vermelho deriving (Show, Eq)

--a)
next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

--b)
stop :: Semaforo -> Bool
stop Vermelho = True
stop otherwise = False

--c)
safe :: Semaforo -> Semaforo -> Bool
safe Verde Vermelho = True
safe Vermelho Verde = True
safe _ _ = False