--ej 1.Definir la funcion nand a b = not (a && b) en Haskell sin utilizar not y &&.

nanD :: Bool -> Bool -> Bool
nanD True True = False
nanD True False = True
nanD False True = True
nanD False False = True 

{- Definir en Haskell la funcion
            maj :: Bool −> Bool −> Bool −> Bool
            retorna True sii al menos 2 argumentos son True
-}

maj :: Bool -> Bool -> Bool -> Bool
maj  True True _  = True
maj  True _ True = True
maj  _ True True = True
maj  otherwise = False 

--paraTodo :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool

--existe 


--sumatoria
sumatoria :: [Int] -> [Int] -> (Int -> [Int] -> Bool) -> Int
sumatoria is xs p = sum[ xs!!i | i<-is , p i xs  ]

--productoria
productoria :: [Int] -> [Int] -> (Int -> [Int] -> Bool) -> Int
productoria is xs p = product[ xs!!i | i<-is , p i xs  ]

--contatoria
contatoria :: [Int] -> [Int] -> (Int -> [Int] -> Bool) -> Int
contatoria is xs p = length[ xs!!i | i<-is , p i xs  ]

--propiedad/predicado
isEven :: Int -> [Int] -> Bool
isEven i xs = mod (xs!!i) 2 == 0



-----------------------------------------------------CUANTIFICADORES----------------------------------------------------------------------
--para todo
paraTodo :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
paraTodo is xs p = and[ p i xs | i<-is ]

--existe
existe :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
existe is xs p = or[ p i xs | i<-is]

--sumatoria
sumatoria :: [Int] -> [Int] -> (Int -> [Int] -> Bool) -> Int
sumatoria is xs p = sum[ xs!!i | i<-is , p i xs  ]

--productoria
productoria :: [Int] -> [Int] -> (Int -> [Int] -> Bool) -> Int
productoria is xs p = product[ xs!!i | i<-is , p i xs  ]

--contatoria
contatoria :: [Int] -> [Int] -> (Int -> [Int] -> Bool) -> Int
contatoria is xs p = length[ xs!!i | i<-is , p i xs  ]

-------------------------------------------------- GUIA 9 --------------------------------------------------------------------------------
--ejercicio 1 

--f es una funcion que determina si los elementos de una lista xs son iguales.

elemIguales :: Eq a => [a] -> Bool
elemIguales xs = and[ xs!!0 == xs!!i | i<-[1..length xs - 1]]

--f es una funcion que determina si los elementos de una lista xs son todos diferentes.

elemDif :: Eq a => [a] -> Bool
elemDif xs = and[ xs!!i /= xs!!j | j<-[0..length xs - 1] , i<-[0..length xs - 1] , i/=j ]

--f es una funcion que determina si los elementos de una lista xs estan ordenados (de menor a mayor))

listOrd :: Ord a => [a] -> Bool
listOrd xs = and[ xs!!i <= xs!!j | i<-[0..length xs - 1] , j<-[0..length xs - 1] , i<j ]

--P es un predicado que es true sii cuando aparece 1 en xs entonces debe aparecer 0 en xs.

unoCero :: Num a => Eq a => [a] -> Bool
unoCero xs = implicacion (or[ xs!!i == 1 | i<-[0..length xs -1]]) (or[ xs!!i == 0 | i<-[0..length xs -1]])


implicacion :: Bool -> Bool -> Bool
implicacion False _ = True
implicacion True x = x 

--p es el producto de todos los elementos primos de xs.
produPrimo :: [Int] -> Int
produPrimo xs = product [ i | i<-[0..length xs -1] , esPrimo (xs!!i) ]

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = and [ (mod n i ) /= 0 | i<- [2..n]]
