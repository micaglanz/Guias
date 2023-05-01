--ej 2. Definir las siguientes funciones:

-- • hd :: [A] -> A retorna el primer elemento de una lista.

hd :: [a] -> a
hd [] = error "head: lista vacía"
hd (x:xs) = x 

-- • tl :: [A] -> [A] retorna toda la lista menos el primer elemento.

tl :: [a] -> [a]
tl [] = error "tail: lista vacía"
tl (x:xs) = xs 

-- • last :: [A] -> A retorna el ultimo elemento de la lista.

last1 :: [a] -> a 
last1 [] = error "last1: lista vacía"
last1 [x] = x 
last1 (x:xs) = last1 xs 

-- • init:: [A] -> [A] retorna toda la lista menos el ultimo elemento.

init1 :: [a] -> [a]
init1 [] = error "init1: lista vacía"
init1 [x] = []
init1 (x:xs) = x : init1 xs 



--ej 3. Defina una funcion maximo de tres, tal que maxTres x y z es el maximo valor entre x, y, z . Por ejemplo: maxTres 6 7 4 = 7.

maxDe3 :: Int -> Int -> Int -> Int
maxDe3 x y z = max (max x y) z  

--ej 4. Defina las siguientes operaciones sobre listas (vistas en el teorico): concatenar, tomar, tirar y triag
      
    --CONCATERNAR
concatenar :: a -> [a] -> [a]
concatenar x [] = [x]
concatenar x xs = x : xs  

    --TOMAR. Funcion que toma los primeros n elementos de la lista en una lista.
    
tomar ::  Int -> [a] -> [a]
tomar n [] = []
tomar 0 xs = []
tomar n (x:xs) = x:(tomar (n-1) xs)

    --TIRAR. Funcion que 'tira' los primeros n elementos de una lista, es decir, devuelve la lista sin los primeros n elementos.
    
tirar :: Int -> [a] -> [a]
tirar n [] = []
tirar 0 xs = xs
tirar n (x:xs) = tirar (n-1) xs 

    --CONCATENACION AL FINAL.
    
concatCola :: a -> [a] -> [a]
concatCola y [] = [y]
concatCola y xs = xs ++ [y] 

--ej 5. Defina una funcion abs: Int -> Int que calcula el valor absoluto de un numero.
abso :: Int -> Int 
abso x | x>0 = x
       | x<0 = -x 
       
--ej 6. Defina una funcion edad :: (Nat,Nat,Nat) -> (Nat,Nat,Nat) -> Int que dada dos fechas indica los años transcurridos entre ellas. Por ejemplo:
--edad (20,10,1968) (30,4,1987) = 18

edad :: (Int,Int,Int) -> (Int,Int,Int) -> Int
edad (d,m,a) (d1,m1,a1) | a>=a1 && m>m1 || a>a1 && m==m1 && d>=d1 = a-a1 
                        | a>=a1 && m<m1 || a>a1 && m==m1 && d<d1  = a-a1-1
                        | a<a1 && m<m1 || a<a1 && m==m1 && d>=d1 = a1-a 
                        | a<a1 && m>m1 || a<a1 && m==m1 && d>d1  = a1-a-1
                        | a==a1 && m==m1 && d==d1 = 0
                        
--ej 7. La disyuncion excluyente xor de dos formulas se verifica si una es verdadera y la otra es falsa. Defina la funcion xor que calcule la disyuncion excluyente a partir de la tabla de verdad.

xOR:: Bool -> Bool -> Bool 
xOR a b | a==True && b==False || a==False && b==True = True
        | a==True && b==True || a==False && b==False = False 
        
--ej 8. Defina una funcion que dado un numero natural, decida si el mismo es primo o no.

primo :: Int -> Bool
primo 1 = False
primo n = and [ (mod n i ) /= 0 | i<- [2..(n-1)]]

--ej 9. Defina una funcion que dado un numero natural n, retorne la lista de todos los numeros naturales primos menores que n.

listaPrimos :: Int -> [Int]
listaPrimos n = [ n |  n<- [1..(n-1)], primo n == True ]

listaPrimos2 :: Int -> [Int]
listaPrimos2 1 = []
listaPrimos2 n = if primo (n-1) then ((n-1) : listaPrimos2 (n-1))
                 else listaPrimos2 (n-1)
                 
--ej 10. Defina una funcion que dada una lista, retorne la reversa de la misma.
reversa :: [a] -> [a]
reversa [] = []
reversa (x:[]) = [x]
reversa (x:xs) = reversa (xs) ++ [x] 

--ej 11. Defina una funcion que dadas dos listas, decida si las listas son iguales.

-- Precondicion, las listas deben estas ordenadas
listasIg :: Eq a => [a] -> [a] -> Bool
listasIg [] [] = True
listasIg _  [] = False
listasIg [] _  = False
listasIg (x:xs) (y:ys) 
         | x == y = listasIg xs ys 
         | otherwise = False 
         
--ej 12. Defina una funcion que dada una lista decida si es un palındromo o no.

palindromo :: 

--ej 13. Defina una funcion que dados tres numeros a, b, c devuelva la cantidad de raıces reales de la ecuacion ax2 + bx + c = 0

raicesReales :: 
