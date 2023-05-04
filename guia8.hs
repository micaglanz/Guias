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
