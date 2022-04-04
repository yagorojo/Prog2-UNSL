{-|
Module      : PracHask2022
Description : Practico Haskell 2022 - Yago Rojo
Developer   :
Maintainer  :

Stability   : experimental
Portability : experimental

Programacion 2 - Practico Haskell 2022
-}

module PracHask2022 (
-- *** Ejercicio 4 - Uso de combinadores básicos sobre funciones primitivas
f4longi, f4longi', f4longi'', f4cuentaE, find, double, double', double', greater, prod, sumsucc, rev, revll,
-- *** Ejercicio 5 – Funciones definidas recursivamente
f5longi, f5cuentaE, f5find, f5double, f5greater, f5prod, f5sumsucc, f5rev, f5revll,
-- *** Ejercicio 6 - Funciones recursivas con acumuladores
f6longi, f6cuentaE, f6double, f6prod, f6sumsucc, f6rev, f6revll,
-- *** Ejercicio 7 - Uso de patrones de recursión predefinidos
f7longi, f7cuentaE, f7find, f7double, f7sumsucc, f7revll,
-- (foldr: catamorfismos sobre estructuras para operadores asociativos a derecha)
-- (foldl: catamorfismos sobre estructuras para operadores asociativos a izquierda)
) where

------------------------------------------------
--
-- Ejercicio 4.
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

--  foldl (:) [] [1,2,3] --> tira error porque [] (:) Num x no se puede.
--  Se pretende lograr algo del estilo (:) Num x []
--  flip :: (a -> b -> c) -> b -> a -> c, usando los mismos parametros;
--  (flip (:)) [] Num x -> Num x : []
--  (flip (:)) [] toma el segundo parametro y obtiene el primero de la llamada
--  de la función.
-- flip (:) [] 3 --> [] (:) 3
-- | Retornar reverso de lista sin reverse (f48)
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

-- | f45 con recursión (f51)
f5greater :: Num p => [a1] -> [a2] -> p
f5greater [] [] = 0
f5greater (_:xs) [] = 1
f5greater [] (_:xs) = 2
f5greater (_:xs) (_:ys) = f5greater xs ys

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
f2 x = f2' [1..x] []
  where
    f2' [] y = y 
    f2' (x:xs) y = x*2 : f2' xs y

-- | (f54)
f' x n = foldr (\h y -> x*h : y) [] [1..n]

------------------------------------------------
--
-- Ejercicio 6.
--
------------------------------------------------

-- |f4longi con acumuladores. (f61)
f6longi :: Num t => [a] -> t
f6longi xs = f6longi' xs 0
  where
    f6longi' [] n = n
    f6longi' (_:xs) n = f6longi' xs (n + 1)

-- | f4cuentaE con acumuladores. (f62)
f6cuentaE :: (Eq t1, Num t2) => [t1] -> t1 -> t2
f6cuentaE xs y = f6cuentaE' xs y 0
  where
    f6cuentaE' [] _ n = n
    f6cuentaE' (x:xs) y n 
      | x == y = f6cuentaE' xs y (n + 1)
      | otherwise = f6cuentaE' xs y n

-- (f63) Resuelto en el ejercicio 5.

-- | double con acumuladores. (f64)
f6double :: [a] -> [a]
f6double xs = f6double' xs []
  where
    f6double' [] l = l
    f6double' (x:xs) l = f6double' xs (x:x:l)

-- | prod con acumuladores. (f66)
f6prod :: Num a => [a] -> a
f6prod [] = 0
f6prod xs = f6prod' xs 1
  where 
    f6prod' [] n = n
    f6prod' (x:xs) n = f6prod' xs (x * n) 

-- | sumsucc con acumuladores. (f67)
f6sumsucc :: [Int] -> Int
f6sumsucc [x, y] = f6sumsucc' (replicate x y) 0
  where
    f6sumsucc' [] n = n
    f6sumsucc' (x:xs) n = f6sumsucc' xs (n + x)

-- | rev con acumuladores. (f68)
f6rev :: [a] -> [a]
f6rev xs = f6rev' xs []
  where
    f6rev' [] l = l
    f6rev' (x:xs) l = f6rev' xs (x:l)

-- | revll con acumuladores. (f69)
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

-- | f4longi con folding, no importa el orden solo consumir la lista, así que usé foldr (f71)
f7longi :: (Foldable t, Num a1) => t a2 -> a1
f7longi = foldr (\_ x -> x + 1) 0 

-- | f4cuentaE con folding, no importa el orden solo consumir la lista, así que usé foldr (f72)
f7cuentaE :: (Foldable t, Eq a1, Num a2) => a1 -> t a1 -> a2
f7cuentaE x xs = foldr (\y acc -> if y == x then acc + 1 else acc) 0 xs

-- | find con folding, no importa el orden solo consumir la lista, así que usé foldr (f73)
f7find :: (Foldable t, Eq a) => a -> t a -> Bool
f7find x xs = foldr (\n acc -> acc || n == x) False xs

-- | double con folding, la lista no requiere orden en particular, solo duplicarlos, así que usé foldr (f74)
f7double :: Foldable t => t a -> [a]
f7double xs = foldr (\n acc -> n:n:acc) [] xs

-- (f76) Resuelto en el ejercicio 4 /f46/

-- | sumsucc con folding, genera una lista y la suma, la suma es asociativa así que usé foldr(f77)
f7sumsucc :: [Int] -> Int
f7sumsucc [a, b] = foldr1 (+) (replicate a b)

-- (f78) Resuelto en el ejercicio 4 /f48/

{-| revll con fold, en este ejercicio importa el orden ya que no quiero alterar el orden de la lista
  si no más bien el orden de sus elementos (listas), que sea asociativo a derecha me permite agregar
  elementos en la lista de ultimo a primero, esto no cambia su orden, si usase foldl se agregarian de
  primero a ultimo resultando en una lista con un reverse (f79)
-}
f7revll :: [[a]] -> [[a]]
f7revll xs = foldr (\x acc -> reverse x : acc) [] xs
