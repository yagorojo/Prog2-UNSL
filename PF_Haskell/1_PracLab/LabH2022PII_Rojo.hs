------------------------------------------------
-- Module    : LabH2022
-- Developer : Rojo Yago
--
-- Programacion II - Laboratorio Haskell 2022
------------------------------------------------

module LabH2022 where
-- Funciones solicitadas en ejs.1,2,3
{- ej. 4.a
Expresion: map item tabla1S
-- Devuelve una lista con los nombres de cada producto en tabla1S
Expresion: filter ((==200).codItem)tabla1S
-- Devuelve una lista con cada elemento que tenga como codigo de producto 200
-}
-- Funcion solicitada en ej.4.a) fReponer
-- Funcion solicitada en ej.4.b) increPU

-------------------------------------------------
-- MODELO SISTEMA DE STOCK DE ALMACEN
-------------------------------------------------

type Cod_Item = Int -- Codigo Interno del producto
type Item = String -- Descripcion del producto
type Marca = String -- Marca
type Rubro = String -- Rubro
type Cod_Proveedor = Int -- Codigo Interno del proveedor
type U_Med = String -- Unidad de Medida:1LT,800GRM, 1500CM3,etc
type Cant_Existente =Int --cantidad de productos en deposito (E)
type V_Min = Int -- valor en existencia recomendado para
-- reposicion (EMin)
type V_Max = Int -- valor maximo de acopio en deposito (EMax)
type Precio_U = Float -- precio o valor de compra unitario
type P_Ganancia = Int -- Porcentaje de ganancia sobre el
-- precio de compra
type Nombre = String
type Direccion = String
type Telefono = String
-- tupla con datos de 1 item de Stock
type Item_Stock = (
  Cod_Item,
  Item,
  Marca,
  Rubro,
  Cod_Proveedor,
  U_Med,
  Cant_Existente,
  V_Min,
  V_Max,
  Precio_U,
  P_Ganancia
  )
-- tupla con datos de 1 proveedor
type Proveedor = (
  Cod_Proveedor,
  Nombre,
  Direccion,
  Telefono
  )
-- Tablas BD
type T_Stock = [Item_Stock] --Tabla con el Stock de un comercio
type T_Proveedor = [ Proveedor] --Tabla con los proveedores de un comercio
--funciones de extracción
codItem (cod,item,marca,rubro,prov,umed,cant,cmin,cmax,preciou,pgan)= cod
item (cod,item,marca,rubro,prov,umed,cant,cmin,cmax,preciou,pgan) = item
precioU (cod,item,marca,rubro,prov,umed,cant,cmin,cmax,preciou,pgan)= preciou
pganancia (cod,item,marca,rubro,prov,umed,cant,cmin,cmax,preciou,pgan)= pgan
cant (cod,item,marca,rubro,prov,umed,cant,cmin,cmax,preciou,pgan)= cant
cmin (cod,item,marca,rubro,prov,umed,cant,cmin,cmax,preciou,pgan)= cmin

-- y así para cada elemento de la tupla(completar el codigo si es necesario)
-- datos predefinidos (Ejemplo)
tabla1S:: T_Stock
tabla1S= [
  (100,"ARROZ GRANO GRANDE","CONDOR","Alimentos",20,"1LT",8000,500,10000,20,30),
  (107,"ARROZ GRANO GRANDE","GALLO","Alimentos",20,"1KG",6000,200,8000,25,30),
  (200,"ACEITE DE GIRASOL","NATURA","Alimentos",20,"1LT",9800,600,10000,40,30),
  (200,"ACEITE DE GIRASOL","COCINERO","Alimentos",20,"1LT",900,500,10000,30,30),
  (410,"AGUA MINERAL S/GAS BAJO SODIO","SER","Alimentos",31,"1.5LT",20,50,3000,10,35),
  (412,"AGUA SABORIZADA LIMA LIMON","SER","Alimentos",31,"2LT",1570,50,3000,15, 35),
  (478,"ALFAJOR CHOCOLATE TITA","TERRABUSI","Alimentos",31,"36GR",900,200,5000,4, 30),
  (479,"ALFAJOR CHOCOLATE RODESIA","TERRABUSI","Alimentos",31,"40GR",9,200,3500, 4,30),
  (708,"LECHE DESC. PASTEURIZADA","SERENISIMA","Alimentos",31,"1TL",230,100,1200,20,30),
  (767,"ARVEJAS SECAS REMOJADAS","NOEL","Alimentos",20,"300GR",1203,500,3000,10,30),
  (801,"ANTITRANSPIRANTE ROLL ON","ETIQUET","PERFUMERIA",20,"60gr",30,45,2000,25,30) ]

tabla1P :: T_Proveedor
tabla1P= [ (20,"Juan Perez","Belgrano 1827, San Luis, 5700, Argentina","2664-786543"),
  (31,"Jose Lopez","Junin 444, Mendoza,5500, Argentina","261-3452677")]

-------------------------------------------------------------------------------
--
-- Ejercicio 1
--
-------------------------------------------------------------------------------

-- En el caso de las recursiones de cola la lista queda revertida, teniendole
-- que hacer un reverse para volverla al orden original, por eso no las hice

mapTup :: (a -> b) -> (c -> d) -> [(a,c)] -> [(b,d)]
mapTup f g xs = map (\ t -> (f (fst t), g (snd t))) xs

-- Recursión controlada.
mapTupR :: (a -> b) -> (c -> d) -> [(a,c)] -> [(b,d)]
mapTupR _ _ [] = []
mapTupR f g (x:xs) = (f (fst x), g (snd x)) : mapTupR f g xs

-- foldr
mapTupfr :: (a -> b) -> (c -> d) -> [(a,c)] -> [(b,d)]
mapTupfr f g xs = foldr (\x acc -> (f (fst x), g (snd x)) : acc) [] xs

-------------------------------------------------------------------------------
--
-- Ejercicio 2
--
-------------------------------------------------------------------------------

allTup :: (a -> Bool) -> (b -> Bool) -> [(a,b)] -> Bool
allTup f g xs = all (\x -> f (fst x) && g (snd x)) xs

-- Recursión controlada.
allTupR :: (a -> Bool) -> (b -> Bool) -> [(a,b)] -> Bool
allTupR _ _ [] = True
allTupR f g (x:xs) = f (fst x) && g (snd x) && allTupR f g xs 

-- Recursión de cola.
allTupRA :: (a -> Bool) -> (b -> Bool) -> [(a,b)] -> Bool
allTupRA f g xs = allTupRA' f g xs True
  where
    allTupRA' _ _ [] a = a
    allTupRA' f g (x:xs) a = allTupRA' f g xs (f (fst x) && g (snd x) && a)

-- foldr
allTupfr :: (a -> Bool) -> (b -> Bool) -> [(a,b)] -> Bool
allTupfr f g xs = foldr (\x acc -> f (fst x) && g (snd x) && acc) True xs

-- foldl
allTupfl :: (a -> Bool) -> (b -> Bool) -> [(a,b)] -> Bool
allTupfl f g xs = foldl (\acc x -> f (fst x) && g (snd x) && acc) True xs

-------------------------------------------------------------------------------
--
-- Ejercicio 3
--
-------------------------------------------------------------------------------

-- Mismo caso que con mapTup.

filterTup :: (a -> Bool) -> (b -> Bool) -> [(a,b)] -> [(a,b)]
filterTup f g xs = filter (\x -> (f (fst x) && g (snd x))) xs

-- Recursión controlada.
filterTupR _ _ [] = []
filterTupR f g (x:xs)
  | (f (fst x) && g (snd x)) = x : filterTupR f g xs
  | otherwise = filterTupR f g xs

-- foldr
filterTupfr :: (a -> Bool) -> (b -> Bool) -> [(a,b)] -> [(a,b)]
filterTupfr f g xs = foldr aux [] xs
  where 
    aux x xs = if f (fst x) && g (snd x) then x : xs else xs

-------------------------------------------------------------------------------
--
-- Ejercicio 4
--
-------------------------------------------------------------------------------

increPU
  :: Fractional j =>
     [(a, b, c, d, e, f, g, h, i, j, k)]
     -> j -> [(a, b, c, d, e, f, g, h, i, j, k)]
increPU xs n = map (\(a,b,c,d,e,f,g,h,i,price,k) -> 
  (a,b,c,d,e,f,g,h,i,(1+n/100)*price,k)) xs

fReponer
  :: Ord g =>
     [(a, b, c, d, e, f, g, g, i, j, k)]
     -> [(a, b, c, d, e, f, g, g, i, j, k)]
fReponer xs = filter (\x -> cant x < cmin x) xs
