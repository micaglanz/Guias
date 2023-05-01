--ejercicios de la guia 2
insertarAlFinal :: [a] -> Int -> [a]
insertarAlFinal [] n = [n]
insertarAlFinal x:xs n = [x:xs]++[n]


insertarAlFinal2 :: [a] -> Int -> [a]
insertarAlFinal2 xs n = [xs]++[n]


maxDeTres ::(Ord a) => a-> a-> a-> a
maxDeTres x y z | (x>=y && x>=z) = x
 | (y>x && y>=z) = y
 | (z>x && z>y) = z 



 

-- Funcion que 'tira' los primeros n elementos de una lista, es decir, devuelve la lista sin los primeros n elementos
tirar :: [a] -> Int -> [a]
tirar [] n = []
tirar xs 0 = xs
tirar (x:xs) n = tirar xs (n-1)

-- Funcion que toma los primeros n elementos de la lista en una lista.
tomar :: [a] -> Int -> [a]
tomar [] n = []
tomar xs 0 = []
tomar (x:xs) n = x:(tomar xs (n-1))



------------------------------------------------------------- GUIA 3 ---------------------------------------------------------------------------------------

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

--otra forma sería cambiar la lìnea   binario n = mod n 2 : (binario (div n 2) y luego apicar la reversa de la lista xq la arma al reves
