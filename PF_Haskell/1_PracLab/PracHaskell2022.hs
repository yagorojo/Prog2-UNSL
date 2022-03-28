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
module PracHask<Año> (
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
-- | double con concatMap.
double' :: Foldable t => t b -> [b]
dobule' = concatMap (replicate 2)
-- | double con concat.
double'' :: [a] -> [a]
double'' xs = concat (replicate xs)
-- | (f45)
greater :: (Ord a, Num p) => a -> a -> p
greater xs ys
  | xs > ys = 1
  | xs < ys = 2
  | otherwise = 0
-- | (f46)
prod :: (Foldable t, Num a) => t a -> a
prod xs = foldr1 (*) xs
-- | (f47)
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

