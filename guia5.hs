--ej 1. Generar una lista infinita de unos
listUno :: [Integer]
listUno = 1:listUno

--ej 2. Generar una lista infinita de naturales comenzando desde un numero dado.

listNat :: Int -> [Int]
listNat n = [n,n+1..]

--ej 3. Generar una lista con los primeros n naturales.

primerosNat :: Int -> [Int]
primerosNat n = [1..n]

--ej 4. Retornar los primeros 5 elementos de una lista infinita de enteros positivos.

primeros5Nat :: [Int] -> [Int]
primeros5Nat xs = take 5 xs           --preguntar como se compila en la terminal, debo citar a xs como una lista inf de enteros? si es asi, Como lo hago?




--*************************************************************** FUNCIONES DE ALTO ORDEN ****************************************************************

-- ej 5. Dada una lista de enteros, retornar sus cuadrados

listCuad :: [Int] -> [Int]
listCuad xs = map (^2) xs 

--ej 6. Dado un entero positivo, retornar la lista de sus divisores.

         -- divs es funcion auxiliar
divs :: Int -> Int -> Bool
divs x y = (mod x y ) == 0

divisores :: Int -> [Int] 
divisores n = filter (divs n) [1..n] -- el divs n funciona haciendo divs n 1 luego, divs n 2 y asi sucesivamente hasta llegar a divs n n. luego el filter se queda con los casos verdaderos 


--ej 7. Dada una lista de naturales, obtener la lista que contenga solo los numeros primos de la lista original.

   --soyPrimo es funcion auxiliar 
soyPrimo :: Int -> Bool
soyPrimo 1 = False
soyPrimo n = and [(mod n i) /= 0 | i<- [2..(n-1)]]


listPrimos :: [Int] -> [Int]
listPrimos xs = filter (soyPrimo) xs

--ej 8. Dada una lista de naturales, retornar la suma de los cuadrados de la lista.

sumCuadList :: [Int] -> Int 
sumCuadList xs = foldr (+) 0 (map (^2) xs)
-- ej9. Dada una lista de naturales, retornar la lista con sus sucesores.
listSucc :: [Int] -> [Int]
listSucc xs = map (+1)  xs

--ej10. Dada una lista de enteros, sumar todos sus elementos.

listInt :: [Int] -> Int
listInt xs = foldr (+) 0 xs

-- ej11. Definir el factorial usando fold
fact :: Int -> Int
fact n = foldr (*) 1 [1..n]

-- ej12. Redefinir la funcion and tal que and xs se verifica si todos los elementos de xs son verdaderos. Por ejemplo: and [1<2, 2<3, 1/=0] = True, and [1<2, 2<3, 1 == 0] = False.

andRd :: [Bool] -> Bool
andRd xs = foldr (and) True xs

--ej13. Usando foldl o foldr definir una funcion tam::[a]->Int que devuelve la cantidad de elementos de una lista dada. Dar un ejemplo en los cuales foldr y foldl evalúen diferente con los mismos parámetros.

contar :: a -> Int -> Int
contar acc = acc + 1

tam :: [a] -> Int
tam xs = foldr contar 0 xs

--************************************************ LISTAS POR COMPRENSIÓN ***********************************************************

--ej14. Dada una lista de enteros, retornar sus sucesores.
listSucc2 :: [Int] -> [Int]
listSucc2 xs = [ (x+1) | x<-xs ]


--ej15. Dada una lista de naturales, retornar sus cuadrados.

listCuad2 :: [Int] -> [Int]
listCuad2 xs = [ (x^2) | x<-xs ]

--ej16. Dada una lista de enteros, retornar los elementos pares que sean mayores a 10.

mayor10 :: [Int] -> [Int]
mayor10 xs = [ x | x<-xs , x>10 ]

--ej17. Dado un entero, retornar sus divisores.

divs :: Int -> Int -> Bool
divs x y = (mod x y) == 0

divisores :: Int -> [Int]
divisores n = [ i | i<-[1..n], divs n i ]
              
--ej18. Definir la funcion todosOcurrenEn :: Eq a => [a] -> [a] -> Bool tal que todosOcurrenEn xs ys se verifica si todos los elementos de xs son elementos de ys. Por ejemplo: todosOcurrenEn [1,5,2,5] [5,1,2,4] = True, todosOcurrenEn [1,5,2,5] [5,2,4] = False

pertenece :: Eq a => a -> [a] -> Bool
pertenece n zs = or [n==z | z<-zs]

todosOcurrenEn :: Eq a => [a] -> [a] -> Bool
todosOcurrenEn xs ys = and [pertenece x ys | x<-xs]

--ej19. Dado un natural n, retornar los numeros primos comprendidos entre 2 y n.

divs :: Int -> Int -> Bool
divs x y = (mod x y) == 0

divisores :: Int -> [Int]
divisores n = [ i | i<-[1..n], divs n i ]

prim :: Int -> Bool --funcion que dado un numero s me devuelve true si es primo o false si no lo es
prim s = if (length(divisores s))>2 then False
         else True


primos :: Int -> [Int]
primos n = [x | x<-[2..n, prim x]]


--ej20. Dadas dos listas de naturales, retornar su producto cartesiano.

productList :: [Int] -> [Int] -> [(Int,Int)]
productList xs ys = [ (x,y) | (x<-xs), (y<-ys) ]

--ej21. Dadas una lista y un elemento retornar el numero de ocurrencias del elemento x en la lista ys.

ocurrencias :: Int -> [Int] -> Int
ocurrencias n xs = length ([x| x<-xs, x==n])

--ej 22. Escribir la funcion split2 :: [a] - > [([a],[a])], que dada una lista xs, devuelve la lista con todas las formas de partir xs en dos. Por ejemplo: split2 [1,2,3] = [([],[1,2,3]), ([1],[2,3]), ([1,2],[3]),([1,2,3],[])].

split2 :: [a] -> [([a],[a])]
split2 xs = [(take n xs , drop n xs) | n<- [0..length (xs)]]

--ej 18, 22, 23 son tipo parcial, averiguar que es nub en haskell

