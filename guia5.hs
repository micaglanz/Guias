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
ocurrencias n xs = [x| x<xs, x==n]

--ej 18, 22, 23 son tipo parcial, averiguar que es nub en haskell

