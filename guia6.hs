--ej 1. Definir el tipo Nat, visto en el teórico.

data Nat = Cero | Suc Nat deriving Show

--ej 2. Definir la funcion natToInt : Nat → Int que dado un numero Nat retorna su entero correspondiente. Por ejemplo: natToInt (Suc(Suc Zero)) = 2.

-- necesito dentro del mismo archivo tener definido el data
--data Nat = Cero | Suc Nat 

natToInt :: Nat -> Int
natToInt Cero = 0 
natToInt (Suc (n)) = 1 + natToInt n 

--ej 3. Definir la funcion intToNat : Int → Nat que dado un numero entero retorna su Nat correspondiente. Por ejemplo: intToNat 2 = (Suc(Suc Zero)).

-- data Nat = Cero | Suc Nat deriving Show --necesito usar el deriving para que sepa que mostrar 

intToNat :: Int -> Nat
intToNat 0 = Cero
intToNat n = Suc (intToNat (n-1))


--ej 4. Definir la funcion sumaNat : Nat → Nat → Nat, la cual suma dos numeros Nat.

sumaNat :: Nat -> Nat -> Nat 
sumaNat Cero Cero = Cero
sumaNat Cero n = n
sumaNat (Suc (m)) n  = Suc ( sumaNat m n )

--ej 5. Definir los arboles binarios.

data Tree a = Nil | Node (Tree a)  a  (Tree a) 
      deriving (Show)

--ej 6. La funcion size, que dado un arbol retorna el numero de nodos del arbol.

size :: Tree a -> Int
size Nil = 0
size (Node hi r hd) = 1 + size hi + size hd

--ej 7. La funcion height, que dado un arbol retorna la altura del mismo.

height :: Tree -> Int
height Nil = 0 
height (Node hi r hd) = 1 + max height hi height hd 


--instanciamos la clase Show 

--instance Show Nat where
-- show Cero = "Cero"
-- show (Suc n) = "Suc" ++ show n 

