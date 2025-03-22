module Testes where

-- TESTES

prodCartBind :: [a] -> [a] -> [(a,a)]
prodCartBind xs ys = xs >>= \x -> ys >>= \y -> [(x,y)]

prodCartDo :: [a] -> [a] -> [(a,a)]
prodCartDo xs ys = do 
    x <- xs
    y <- ys
    [(x,y)]

prodCartComp :: [a] -> [a] -> [(a,a)]
prodCartComp xs ys = [(x,y) | x <- xs, y <- ys]

pyths :: Int -> [(Int,Int,Int)]
pyths n = [1  ..n] >>= \x ->
          [x+1..n] >>= \y ->
          [y+1..n] >>= \z -> if x^2 + y^2 == z^2 then [(x,y,z)] else []

-- Alternativa ao operador >>=
(<||>) :: [a] -> (a -> [b]) -> [b]
[]     <||> _ = []
(x:xs) <||> f = f x ++ xs <||> f

-- Alternativa ao operador <*>
(<**>) :: [a -> b] -> [a] -> [b]
[]     <**> _      = []
(f:fs) <**> xs = f <$$> xs ++ fs <**> xs

-- Alternativa ao operador <$>
(<$$>) :: (a -> b) -> [a] -> [b]
_ <$$> [] = []
f <$$> (x:xs) = f x : (f <$$> xs)

-- Alternativa à função length

myLength :: [a] -> Int
myLength = foldl (\x _ -> x + 1) 0 

insereFim :: a -> [a] -> [a]
insereFim x [] = [x]
insereFim x (y:ys) = y : insereFim x ys
