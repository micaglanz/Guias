--ej 1. Define una funcion que, dadas dos listas ys y xs de naturales ordenadas, retorne el merge de estas listas, es decir, la lista ordenada compuesta por los elementos de ys y xs.

-- las listas de numeros naturales deben igresar se forma ordenda
merge :: [Int]->[Int]->[Int]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x <= y 
                      then x:merge xs (y:ys)
                      else y:merge (x:xs) ys
                      
--ej 2. función que dada un lista de naturales retorna la lista ordenada

-- podría elegir cualquier método de ordenamiento (bubble sort no es eficiente pero sirve)

ordBS :: Int->[Int]->[Int]
ordBS 0 xs = xs
ordBS n (xs) = ordBS (n-1) (bubble xs)
      where 
      bubble :: [Int]->[Int]
      bubble [] = []
      bubble (x : []) = [x]
      bubble (x : y : ys) = if x <= y 
                        then x : (bubble (y:ys))
                        else y : (bubble (x:ys))
                        
                        
bubbleSort :: [Int]->[Int]
bubbleSort xs = let n = length xs
                in ordBS n xs
                
--ej 3. Define una función que, recursivamente y solo utilizando adición y multiplicación, calcule, dado un natural n, el numero 2^n

pot2 :: Int->Int
pot2 0 = 1
pot2 n = 2*pot2(n-1) 

--ej 4. Define una función que, dado un numero natural n, retorne su representación binaria como secuencia de bits

binario :: Int->[Int]
binario 0 = []
binario 1 = [1]
binario n = binario (div n 2) ++ [mod n 2]

      --otra forma sería cambiar la lìnea   binario n = mod n 2 : (binario (div n 2) y luego apicar la reversa de la lista xq la arma al  reves

--ej 5. Define una funcion que, dado un numero natural n en su representacion binaria, decida si n es par o no.


--ej 6. Define la funcion que retorne la distancia de Hamming: dadas dos listas es el numero de posiciones en que los correspondientes elementos son distintos. Por ejemplo: distanciaH ”roma””camino”− > 3  distanciaH ”romano””rama”− > 1


--ej 7. Define la funcion que, dado un numero natural, decida si el mismo es un cuadrado perfecto o no.


--ej 8. Define la funcion repetidos de forma tal que dado un elemento z y un enteron; z aparece n veces.


--ej 9. Define la funcion nelem tal que nelem xs n es elemento enesimo de xs, empezando a numerar desde el 0. Por ejemplo: nelem [1, 3, 2, 4, 9, 7]3− > 4


--ej 10 *. Define la funcion posicionesC tal que posicionesC xs c es la lista de la posiciones del caracter c en la cadena xs. Por ejemplo: posicionesC ”Catamarca” 0 a 0− > [1, 3, 5, 8]



--ej 11. Define la funcion compact, dada una lista retorna la lista sin los elementos repetidos consecutivos. Por ejemplo: compact [1, 3, 3, 5, 8, 3] = [1, 3, 5, 8, 3]

