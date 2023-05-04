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

{-
usuario@101-2:~/Documentos/avanzada$ git init
ayuda: Usando 'master' como el nombre de la rama inicial. Este nombre de rama predeterminado
ayuda: está sujeto a cambios. Para configurar el nombre de la rama inicial para usar en todos
ayuda: de sus nuevos repositorios, reprimiendo esta advertencia, llama a:
ayuda: 
ayuda: 	git config --global init.defaultBranch <nombre>
ayuda: 
ayuda: Los nombres comúnmente elegidos en lugar de 'master' son 'main', 'trunk' y
ayuda: 'development'. Se puede cambiar el nombre de la rama recién creada mediante este comando:
ayuda: 
ayuda: 	git branch -m <nombre>
Inicializado repositorio Git vacío en /home/usuario/Documentos/avanzada/.git/
usuario@101-2:~/Documentos/avanzada$ git add guia8.hs
usuario@101-2:~/Documentos/avanzada$ git status
En la rama master

No hay commits todavía

Cambios a ser confirmados:
  (usa "git rm --cached <archivo>..." para sacar del área de stage)
	nuevos archivos: guia8.hs

usuario@101-2:~/Documentos/avanzada$ git commit
Abortando commit debido que el mensaje está en blanco.
usuario@101-2:~/Documentos/avanzada$ git commit -m "primera parte del repo"
[master (commit-raíz) 7b09e7d] primera parte del repo
 1 file changed, 18 insertions(+)
 create mode 100644 guia8.hs
usuario@101-2:~/Documentos/avanzada$ git push
fatal: No se ha configurado un destino para el empuje.
Puedes o especificar una URL desde la línea de comandos o configurar un repositorio remoto usando

    git remote add <nombre> <url>

y luego empujar al nombre del remoto

    git push <nombre>

usuario@101-2:~/Documentos/avanzada$ git remote add <micaglanz> <https://github.com/micaglanz/Guias>
bash: error sintáctico cerca del elemento inesperado `<'
usuario@101-2:~/Documentos/avanzada$ git remote add micaglanz https://github.com/micaglanz/Guias
usuario@101-2:~/Documentos/avanzada$ git push guia8.hs
fatal: formato gitfile inválido: guia8.hs
fatal: No se pudo leer del repositorio remoto.

Por favor asegúrate de que tengas los permisos de acceso correctos
y que el repositorio exista.
usuario@101-2:~/Documentos/avanzada$ git push micaglanz
fatal: The current branch master has no upstream branch.
To push the current branch and set the remote as upstream, use

    git push --set-upstream micaglanz master

To have this happen automatically for branches without a tracking
upstream, see 'push.autoSetupRemote' in 'git help config'.
usuario@101-2:~/Documentos/avanzada$ git push 
fatal: The current branch master has no upstream branch.
To push the current branch and set the remote as upstream, use

    git push --set-upstream micaglanz master

To have this happen automatically for branches without a tracking
upstream, see 'push.autoSetupRemote' in 'git help config'.

usuario@101-2:~/Documentos/avanzada$ git remote add  micaglanz https://github.com/micaglanz/Guias
error: remoto micaglanz ya existe.
usuario@101-2:~/Documentos/avanzada$ git status
En la rama master
nada para hacer commit, el árbol de trabajo está limpio
usuario@101-2:~/Documentos/avanzada$ git log
commit 7b09e7d0dfcb6ea86878917da08f15b3d13db259 (HEAD -> master)
Author: gabrielbaseggio <baseggioaxel@gmail.com>
Date:   Thu May 4 06:32:42 2023 -0300

    primera parte del repo
usuario@101-2:~/Documentos/avanzada$ git push
fatal: The current branch master has no upstream branch.
To push the current branch and set the remote as upstream, use

    git push --set-upstream micaglanz master

To have this happen automatically for branches without a tracking
upstream, see 'push.autoSetupRemote' in 'git help config'.

usuario@101-2:~/Documentos/avanzada$ git config --global user.name
gabrielbaseggio
usuario@101-2:~/Documentos/avanzada$ git config --global user.name micaglanz
usuario@101-2:~/Documentos/avanzada$ git config --global user.email mica.glanzmann@gmail.com
usuario@101-2:~/Documentos/avanzada$ git clone https://github.com/micaglanz/Guias.git
Clonando en 'Guias'...
remote: Enumerating objects: 35, done.
remote: Counting objects: 100% (35/35), done.
remote: Compressing objects: 100% (32/32), done.
remote: Total 35 (delta 9), reused 0 (delta 0), pack-reused 0
Recibiendo objetos: 100% (35/35), 13.16 KiB | 313.00 KiB/s, listo.
Resolviendo deltas: 100% (9/9), listo.
usuario@101-2:~/Documentos/avanzada$ cd Guias/
usuario@101-2:~/Documentos/avanzada/Guias$ git status
En la rama main
Tu rama está actualizada con 'origin/main'.

nada para hacer commit, el árbol de trabajo está limpio
usuario@101-2:~/Documentos/avanzada/Guias$ git status
En la rama main
Tu rama está actualizada con 'origin/main'.

Cambios no rastreados para el commit:
  (usa "git add <archivo>..." para actualizar lo que será confirmado)
  (usa "git restore <archivo>..." para descartar los cambios en el directorio de trabajo)
	modificados:     guia8.hs

sin cambios agregados al commit (usa "git add" y/o "git commit -a")
usuario@101-2:~/Documentos/avanzada/Guias$ git add guia8.hs 
usuario@101-2:~/Documentos/avanzada/Guias$ git commit -m "ejercicios 1 y 2"
[main 812a375] ejercicios 1 y 2
 1 file changed, 15 insertions(+), 31 deletions(-)
usuario@101-2:~/Documentos/avanzada/Guias$ git push 
Username for 'https://github.com': micaglanz
Password for 'https://micaglanz@github.com': 
remote: Support for password authentication was removed on August 13, 2021.
remote: Please see https://docs.github.com/en/get-started/getting-started-with-git/about-remote-repositories#cloning-with-https-urls for information on currently recommended modes of authentication.
fatal: Autenticación falló para 'https://github.com/micaglanz/Guias.git/'
usuario@101-2:~/Documentos/avanzada/Guias$ git push
Username for 'https://github.com': micaglanz
Password for 'https://micaglanz@github.com': 
Enumerando objetos: 5, listo.
Contando objetos: 100% (5/5), listo.
Compresión delta usando hasta 4 hilos
Comprimiendo objetos: 100% (3/3), listo.
Escribiendo objetos: 100% (3/3), 485 bytes | 485.00 KiB/s, listo.
Total 3 (delta 1), reusados 0 (delta 0), pack-reusados 0
remote: Resolving deltas: 100% (1/1), completed with 1 local object.
To https://github.com/micaglanz/Guias.git
   7ef5439..812a375  main -> main
usuario@101-2:~/Documentos/avanzada/Guias$ 


-}
