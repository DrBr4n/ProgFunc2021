import Data.Char
import Data.List


--Ficha 1

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
stop otherwise =  False

--c)
safe :: Semaforo -> Semaforo -> Bool
safe Verde Vermelho = True
safe Vermelho Verde = True
safe _ _ = False

--6

data Pont = Cartesiano Double Double | Polar Double Double deriving(Show, Eq)

--a)
posx :: Pont -> Double
posx (Cartesiano x y) = x
--posx (Polar r a) = r * cos(a)

--b)
posy :: Pont -> Double
posy (Cartesiano x y) = y
--posy (Polar r a) = r * sin(a)

--c)
raio :: Pont -> Double
raio (Cartesiano x y) = sqrt(x^2 + y^2)
raio (Polar r a) = r


--Ficha 2

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

--e)
calcula :: Float -> Polinomio -> Float 
calcula n [] = 0
calcula n ((c,e):t) = c * n^e + calcula n t

--f)
simp :: Polinomio -> Polinomio
simp [] = []
simp (h:t) | fst h == 0 = simp t
        | otherwise = h : simp t

--l)
equiv :: Polinomio -> Polinomio -> Bool
equiv a b | calcula 2 a == calcula 2 b = True 
        | otherwise = False


--Ficha 3

-- data Hora = H Int Int deriving Show
type Etapa = (Hora, Hora)
type Viagem =  [Etapa]

--1
--a)
etapaValida :: Etapa -> Bool
etapaValida (p,c) | valida p && valida c && depois c p = True
                  | otherwise = False

--b)
viagemValida :: Viagem -> Bool
viagemValida [] = True
viagemValida [e] | etapaValida e = True
                | otherwise = False
viagemValida (e1:e2:es) | etapaValida e1 && depois (fst e2) (snd e1) = viagemValida (e2:es)
                        | otherwise = False

--c)
horas :: Viagem -> (Hora, Hora)
horas [e] = (fst e, snd e)
horas (e:es) = (fst e, snd (last es))

--d)
total :: Viagem -> Hora
total [e] = converteM (converte (snd e) - converte (fst e))
total (e:es) = converteM (converte (snd (last es)) - converte (fst e))


--Ficha 4

--3
digitAlpha :: String -> (String, String)
digitAlpha [] = ([], [])
digitAlpha (h:t) | isDigit h = (h : fst(digitAlpha t), snd(digitAlpha t))
                 | isAlpha h = (fst(digitAlpha t), h : snd(digitAlpha t)) 

--4
nzp :: [Int] -> (Int, Int, Int)
nzp [] = (0, 0, 0)
nzp (h:t) | h > 0  = (0 + fs, 0 + sn, 1 + tr) 
          | h == 0 = (0 + fs, 1 + sn, 0 + tr)  
          | h < 0  = (1 + fs, 0 + sn, 0 + tr)
    where fs = aa (nzp t) 
          sn = bb (nzp t)
          tr = cc (nzp t)

--5
divmod :: Integral a => a -> a -> (a, a)
divmod x y 
        | x > y =  (1 + fst(divmod (x-y) y), 0 + snd(divmod (x-y) y))
        | x == y = (1, 0)
        | x < y =  (0, x)

--6
fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits l = fromDigitsAux (length l-1) l

fromDigitsAux :: Int -> [Int] -> Int
fromDigitsAux _ [] = 0
fromDigitsAux ac (h:t) = fromDigitsAux (ac-1) t + (h * 10^ac)


--Ficha 5

--1
--a)
any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (x:xs) | f x = True
              | otherwise = any' f xs

--b)
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

--c)
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) | f x = x : takeWhile' f xs
                    | otherwise = []

--d)
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs) | f x = dropWhile' f xs
                    | otherwise = (x:xs)

--e)
span' :: (a -> Bool) -> [a] -> ([a],[a])
span' _ [] = ([],[])
span' f (x:xs) | f x = (x : fst(span' f xs), snd(span' f xs))
               | otherwise = ([], (x:xs))

--f)
deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' _ _ [] = []
deleteBy' f v (x:xs) | f v x = xs
                     | otherwise = x : deleteBy' f v xs


--Ficha 6

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show
x = (Node 4 (Node 3 (Node 1 (Node 2 Empty Empty) Empty) Empty) (Node 5 (Node 7 Empty Empty) (Node 9 Empty Empty)))
y = (Node 4 (Node 3 (Node 1 (Node 2 Empty Empty) Empty) (Node 8 Empty Empty)) (Node 5 (Node 7 Empty Empty) (Node 9 Empty Empty)))
z = (Node 6 (Node 3 (Node 1 Empty Empty) Empty) (Node 10 (Node 9 Empty Empty) (Node 12 Empty Empty)))
--1
--a
altura :: BTree a -> Int
altura Empty = 0
altura (Node a l r) = 1 + max (altura l) (altura r)

--b
contaNodos :: BTree a -> Int
contaNodos Empty = 0 
contaNodos (Node a l r) = 1 + contaNodos l + contaNodos r 

--c
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node a Empty Empty) = 1
folhas (Node a l r) = folhas l + folhas r

--d
prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 (Node a l r) = Empty
prune n (Node a l r) = (Node a (prune (n-1) l) (prune (n-1) r))

--e
path :: [Bool] -> BTree a -> [a]
path [] (Node a l r) = [a]
path _ Empty = []
path (h:t) (Node a l r) | h = a : path t r
                        | not h = a : path t l

--f
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node a l r) = Node a (mirror r) (mirror l)

--g
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node a l r) (Node aa ll rr) = Node (f a aa) (zipWithBT f l ll) (zipWithBT f r rr)
zipWithBT _ _ _ = Empty

--h
unzipBT :: BTree (a, b, c) -> (BTree a, BTree b, BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (a, b, c) l r) = (Node a l1 r1, Node b l2 r2, Node c l3 r3)
        where (l1, l2, l3) = unzipBT l
              (r1, r2, r3) = unzipBT r

--2
--a
minimo :: Ord a => BTree a -> a
minimo (Node a Empty _) = a
minimo (Node a l r) = minimo l

--b
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node a Empty _) = Empty
semMinimo (Node a l r) = (Node a (semMinimo l) r)

--c
minSmin :: Ord a => BTree a -> (a, BTree a)
minSmin (Node a Empty _) = (a, Empty)
minSmin (Node a l r) =  (fst(minSmin l), Node a (snd(minSmin l)) r)

--3
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int | Rep | Faltou deriving Show
type Turma = BTree Aluno -- ´arvore bin´aria de procura (ordenada por n´umero)

turma1 :: Turma
turma1 = (Node (15,"Luís",ORD,Aprov 14) (Node (12,"Joana",MEL,Faltou) (Node (7,"Diogo",TE,Rep) Empty
                                                                                               Empty) 
                                                                      (Node (14,"Lara",ORD,Aprov 19) Empty
                                                                                                     Empty))
                                        (Node (20,"Pedro",TE,Aprov 10) Empty
                                                                       (Node (25,"Sofia",ORD,Aprov 20) (Node (23,"Rita",ORD,Aprov 17) Empty
                                                                                                                                      Empty)
                                                                                                       (Node (28,"Vasco",MEL,Rep) Empty
                                                                                                                                  Empty))))

--a
inscNum :: Numero -> Turma -> Bool
inscNum n Empty = False
inscNum n (Node (a,_,_,_) l r) | n < a = inscNum n l 
                               | n > a = inscNum n r
                               | n == a = True

--b
inscNome :: Nome -> Turma -> Bool
inscNome n (Node (_,name,_,_) l r) | n == name || inscNome n l || inscNome n r

--c
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,name,reg,_) l r) | (_,_,TE,_) = [(num,name)] ++ trabEst l ++ trabEst r
                                    | otherwise = []