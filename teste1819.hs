--1
--a)
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' x l = elemIndices'' 0 x l
    where elemIndices'' :: Eq a => Int -> a -> [a] -> [Int]
          elemIndices'' _ _ [] = []
          elemIndices'' n x (h:t)
                | x == h = n : elemIndices'' (n+1) x t
                | otherwise = elemIndices'' (n+1) x t

--b)
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] [] = True

isSubsequenceOf' [] l = True
isSubsequenceOf' l [] = False
isSubsequenceOf' (x:xs) (y:ys)
      | x == y = isSubsequenceOf' xs ys
      | otherwise = isSubsequenceOf'(x:xs) ys

--2)
data BTree a = Empty | Node a (BTree a) (BTree a)

--a)
inOrder' :: BTree a -> [a]
inOrder' Empty = []
inOrder' (Node a l r) = inOrder' l ++ [a] ++ inOrder' r

preOrder' :: BTree a -> [a]
preOrder' Empty = []
preOrder' (Node a l r) = [a] ++ preOrder' l ++ preOrder' r

posOrder' :: BTree a -> [a]
posOrder' Empty = []
posOrder' (Node a l r) = posOrder' l ++ posOrder' r ++ [a]