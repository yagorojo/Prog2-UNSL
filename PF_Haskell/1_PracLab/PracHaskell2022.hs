------------------------------------------------
-- Module : PracHask<Año>
-- Developer :
-- Maintainer :
-- Stability : experimental
-- Portability : experimental
--
-- <Materia> - Practico Haskell <Año>
------------------------------------------------

-- | Este modulo contiene las definiciones de las funciones solicitadas en el practico Haskell
module PracHask2022 (
-- *** Ejercicio 4 - Uso de combinadores básicos sobre funciones primitivas f4longi,f4longi', f4longi'', f4cuentaE Completar
-- *** Ejercicio 5 – Funciones definidas recursivamente
-- Completar (f6genPares,...)
-- *** Ejercicio 6 - Funciones recursivas con acumuladores
-- *** Ejercicio 7 - Uso de patrones de recursión predefinidos
-- (foldr: catamorfismos sobre estructuras para operadores asociativos a derecha)
-- (foldl: catamorfismos sobre estructuras paraoperadores asociativos a izquierda)
) where
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
f4cuentaE e l = f5longi (filter (==e) l)
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
-- | double con concat (f44)
double''' :: [a] -> [a]
double''' xs = concat (replicate xs)
-- | Retorna 1 si xs > xy, 2 si xs < xy, 0 si xs = xy (f45)
greater :: (Ord a, Num p) => a -> a -> p
greater xs ys | xs > ys = 1
  | xs < ys = 2
  | otherwise = 0
-- | Retorna producto de átomos numéricos (f46)
prod :: (Foldable t, Num a) => t a -> a
prod xs = foldr1 (*) xs
-- | Tomando [a, b] retorna a * b con sumas sucesivas (f47)
sumsucc :: [Int] -> Int
sumsucc [x, y] = sum (take 2 (repeat 3)) 
-- | Retornar reverso de lista sin reverse (f48)
--  foldl (:) [] [1,2,3] --> tira error porque [] (:) Num x no se puede.
--  Se pretende lograr algo del estilo (:) Num x []
--  flip :: (a -> b -> c) -> b -> a -> c, usando los mismos parametros;
--  (flip (:)) [] Num x -> Num x : []
--  (flip (:)) [] toma el segundo parametro y obtiene el primero de la llamada
--  de la función.
-- flip (:) [] 3 --> [] (:) 3
rev :: Foldable t => [t a] -> [a]
rev = foldl (flip (:)) []
-- | Devuelve lista de listas invertidas (f49)
revll :: [[a]] -> [[a]]
revll = map reverse

-- *** Ejercicio 5:
-- | f41 con recursión (f51) 
f5longi :: [a] -> Int
f5longi [] = 0
f5longi (_:xs) = 1 + f5longi xs
-- | f42 con recursión (f51) DEFINIDA LINEA 32
-- | f43 con recursión (f51)
f5find :: [a] -> a -> Bool
f5find [] _ = False
f5find (x:xs) e = x || f5find xs
-- | f44 con recursión (f51)
f5double ::  [a] -> [a]
f5double [] = []
f5double (x:xs) = x:x:f5double
-- | f45 con recursión (f51)
f5greater :: [a1] -> [a3] -> Bool
f5greater xs xy | len = 1
  | (not) len = 2
  | otherwise = 0
  where len = f5longi xs > f5longi xt
-- | f46 con recursión (f51)
f5prod :: Num a => [a] -> a
f5prod [x] = x
f5prod (x:xs) = x * f5prod xs
-- | f47 con recursión (f51)
f5sumsucc [_, 0] = 0
f5sumsucc [x, 1] = x
-- fix
f5sumsucc [x, y] = x + f5sumsucc [x, (y - 1)] 
-- | f48 con recursión (f51)
f5rev :: [a] -> [a]
f5rev [] = []
f5rev (x:xs) = f5rev xs ++ [x] 
-- | f49 con recursión (f51)
f5revll :: [[a]] -> [[a]]
f5revll [] = []
f5revll (x:xs) = reverse x : f5revll xs 
