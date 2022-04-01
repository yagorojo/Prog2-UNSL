------------------------------------------------
-- Module : PracHask2022
-- Developer :
-- Maintainer :
-- Stability : experimental
-- Portability : experimental
--
-- <Materia> - Practico Haskell <Año>
------------------------------------------------

module PracHask2022 () where

------------------------------------------------
--
--  Ejercicio 4.
--
------------------------------------------------

-- | Calcula la longitud de una lista (f41)
f4longi :: [a] -> Integer
f4longi l = sum (map (const 1) l)

-- | f4longi usando composicion (f41)
f4longi' :: [a] -> Integer
f4longi' l = (sum . (map (const 1))) l

-- | f4longi pointfree (f41)
f4longi'' :: [a] -> Integer
f4longi'' = (sum . (map (const 1)))

-- | Cuenta cantidad de elementos iguales al 1er. arg. en una lista (f42)
f4cuentaE :: (Eq a) => a -> [a] -> Integer
f4cuentaE e l = f4longi (filter (==e) l)

-- | Retorna bool indicando si se encuentra el elemento en la lista (f43)
find :: (Foldable t, Eq a) => a -> t a -> Bool
find s xs = elem s xs

-- | Genera lista a partir de otra donde cada elemento figura dos veces (f44)
double :: [a] -> [a]
double xs = take ((length xs) * 2) (cycle xs)

-- | doule con ++ (f44)
double' :: [a] -> [a]
double' xs = (++) xs xs

-- | double con concatMap (f44)
double'' :: Foldable t => t b -> [b]
double'' = concatMap (replicate 2)

-- | Retorna 1 si xs > xy, 2 si xs < xy, 0 si xs = xy (f45)
greater :: (Ord a, Num p) => a -> a -> p
greater xs ys 
  | xs > ys = 1
  | xs < ys = 2
  | otherwise = 0

-- | Retorna producto de átomos numéricos (f46)
prod :: (Foldable t, Num a) => t a -> a
prod xs = foldr1 (*) xs

-- | Tomando [a, b] retorna a * b con sumas sucesivas (f47)
sumsucc :: [Int] -> Int
sumsucc [x, y] = sum (take x (repeat y)) 

-- | Retornar reverso de lista sin reverse (f48)
--  foldl (:) [] [1,2,3] --> tira error porque [] (:) Num x no se puede.
--  Se pretende lograr algo del estilo (:) Num x []
--  flip :: (a -> b -> c) -> b -> a -> c, usando los mismos parametros;
--  (flip (:)) [] Num x -> Num x : []
--  (flip (:)) [] toma el segundo parametro y obtiene el primero de la llamada
--  de la función.
-- flip (:) [] 3 --> [] (:) 3
rev :: Foldable t => t a -> [a]
rev = foldl (flip (:)) []

-- | Devuelve lista de listas invertidas (f49)
revll :: [[a]] -> [[a]]
revll = map reverse

------------------------------------------------
--
-- Ejercicio 5.
--
------------------------------------------------

-- | f41 con recursión (f51) 
f5longi :: [a] -> Int
f5longi [] = 0
f5longi (_:xs) = 1 + f5longi xs

-- | f42 con recursión (f51)
f5cuentaE :: (Num p, Eq t) => t -> [t] -> p
f5cuentaE y [] = 0
f5cuentaE y (x:xs)
  | x == y = 1 + f5cuentaE y xs
  | otherwise = f5cuentaE y xs

-- | f43 con recursión (f51)
f5find _ [] = False
f5find y (x:xs) 
  | y == x = True
  | otherwise = find y xs

-- | f44 con recursión (f51)
f5double ::  [a] -> [a]
f5double [] = []
f5double (x:xs) = x:x:f5double xs

-- | f46 con recursión (f51)
f5prod :: Num a => [a] -> a
f5prod [x] = x
f5prod (x:xs) = x * f5prod xs

-- | f47 con recursión (f51)
f5sumsucc :: (Eq a, Num a) => [a] -> a
f5sumsucc [_, 0] = 0
f5sumsucc [x, 1] = x
f5sumsucc [x, y] = x + f5sumsucc [x, y - 1]

-- | f48 con recursión (f51)
f5rev :: [a] -> [a]
f5rev [] = []
f5rev (x:xs) = f5rev xs ++ [x] 

-- | f49 con recursión (f51)
f5revll :: [[a]] -> [[a]]
f5revll [] = []
f5revll (x:xs) = reverse x : f5revll xs 

-- | (f53)


------------------------------------------------
--
-- Ejercicio 6.
--
------------------------------------------------

-- | (f61)
f6longi :: Num t => [a] -> t
f6longi xs = f6longi' xs 0
  where
    f6longi' [] n = n
    f6longi' (_:xs) n = f6longi' xs (n + 1)

-- | (f62)
f6cuentaE :: (Eq t1, Num t2) => [t1] -> t1 -> t2
f6cuentaE xs y = f6cuentaE' xs y 0
  where
    f6cuentaE' [] _ n = n
    f6cuentaE' (x:xs) y n 
      | x == y = f6cuentaE' xs y (n + 1)
      | otherwise = f6cuentaE' xs y n

-- | (f63) Resuelto en el ejercicio 5.

-- | (f64)
f6double :: [a] -> [a]
f6double xs = f6double' xs []
  where
    f6double' [] l = l
    f6double' (x:xs) l = f6double' xs (x:x:l)

-- | (f65)

-- | (f66)
f6prod :: Num a => [a] -> a
f6prod [] = 0
f6prod xs = f6prod' xs 1
  where 
    f6prod' [] n = n
    f6prod' (x:xs) n = f6prod' xs (x * n) 

-- | (f67)
f6sumsucc :: [Int] -> Int
f6sumsucc [x, y] = f6sumsucc' (replicate x y) 0
  where
    f6sumsucc' [] n = n
    f6sumsucc' (x:xs) n = f6sumsucc' xs (n + x)

-- | (f68)
f6rev :: [a] -> [a]
f6rev xs = f6rev' xs []
  where
    f6rev' [] l = l
    f6rev' (x:xs) l = f6rev' xs (x:l)

-- | (f69)
f6revll :: [[a]] -> [[a]]
f6revll xs = f6revll' xs []
  where
    f6revll' [] l = l
    f6revll' (x:xs) l = f6revll' xs $ reverse x : l

------------------------------------------------
--
-- Ejercicio 7.
--
------------------------------------------------

-- | (f71)
f7longi :: (Foldable t, Num a1) => t a2 -> a1
f7longi = foldr (\_ x -> x + 1) 0 

-- | (f72)
f7cuentaE x xs = foldr (==x) 0 xs

-- | (f78) Resuelto en el ejercicio 4 /f48/
