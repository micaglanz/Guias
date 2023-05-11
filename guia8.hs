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
