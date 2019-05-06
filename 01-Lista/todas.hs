-- 01 menor entre x y
menord2::Int -> Int -> Int
menord2 x y = if x <= y then x else y

-- 02 menor entre x y z
menord3::Int -> Int -> Int -> Int
menord3 x y z   | x <= y = x
                | y <= z = y
                | otherwise = z

-- 03 fatorial
fat::Int -> Int
fat x = product [1 .. x]

-- 04 fibonacci
fib::Int -> Int
fib 1 = 0 -- condição de parada para a recursão
fib 2 = 1 -- condição de parada para a recursão
fib x = fib (x - 1) + fib (x - 2)

-- 05 elemento
elemento::Int -> [Int] -> Int
elemento x xs = xs !! x

-- 06 pertence 
pertence::Int -> [Int] -> Bool
pertence x xs = x `elem` xs

-- 07 total
total::[Int] -> Int
total [] = 0 -- condição de parada para a recursão
total [x] = 1 -- condição de parada para a recursão
total xs = total (tail xs) + 1

-- 08 maior
maior::[Int] -> Int
maior [x] = x -- condição de parada para a recursão
maior (x:y:resto)   | x > y = maior (x: resto)
                    | otherwise = maior (y: resto)

-- 09 frequencia
frequencia::Int -> [Int] -> Int
frequencia x [] = 0 -- condição de parada para a recursão
frequencia x (u:us) | x==u = 1 + frequencia (x) (us)
                    | otherwise = frequencia (x) (us)

-- 10 unico
unica::Int -> [Int] -> Bool
unica (x) (us) = if (frequencia x us) == 1
                then True else False

-- 11 maioresQue
maioresQue::Int -> [Int] -> [Int]
maioresQue x us = [u | u<-us, u > x]

-- 12 concat
concatI::[Int] -> [Int] -> [Int]
concatI [] [] = []
concatI as bs = as++bs

-- 13 calda
calda::[Int] -> [Int]
calda us = tail us

-- 14 corpo
corpo::[Int] -> [Int]
corpo us = init us

-- 15 unique
unique::[Int] -> [Int]
unique [] = []
unique [x] = [x]
unique (y : ys) = if y `elem` ys 
    then unique ys else y : unique ys

-- 16 menores

-- 17 alter
alter::Int -> [Int]
alter 0 = [0]
alter a = alter(c - 1)++ [c , (-c)]
    where c = a

-- 18 reverso
reverseMy::[Int] -> [Int]
reverseMy [] = []
reverseMy [a] = [a]
reverseMy xs = (last xs) : reverseMy (init xs)

-- 19 divide
divide::[Int] ->
     Int -> ([Int],[Int])
divide us x = (take x us, drop x us)

-- 20 intercal
intercal::[Int] -> [Int] -> [Int]
intercal [] [] = []
intercal a [] = a
intercal [] b = b
intercal (a:as) (bs) = a:intercal (bs) (as)

-- 21 uniao
uniao::[Int] -> [Int] -> [Int]
uniao as bs = unique (concatI as bs)

-- 22 intersec
intersec::[Int] -> [Int] -> [Int]
intersec [] _ = []
intersec [a] _ = [a]
intersec (a:as) bs = if a `elem` bs 
    then a : intersec (as) (bs) else intersec (as) (bs)

-- 23 sequencia
sequencia::Int -> Int -> [Int]
sequencia n m = [m,(m+1)..(m+n-1)]

-- 24 inserir
inserir::Int -> [Int] -> [Int]
inserir _ [] = []
inserir _ [a] = [a]
inserir x us = (a ++ [x] ++ b) 
    where   a = takeWhile ( <= x) us
            b = tail $ dropWhile (>=x) us

-- 25 isSorted
isSorted::[Int] -> Bool
isSorted [] = True
isSorted [a] = True
isSorted (u:y:us)   | u<=y = isSorted (y:us)
                    | otherwise = False

-- 26 qsort
quicksort::[Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort small ++ (x : quicksort large)
    where small = [y | y <- xs, y <= x]
          large = [y | y <- xs, y > x]

-- 27 rotEsq
rotEsq::Int -> [Char] -> [Char]
rotEsq n (u:us) | n > 0 = rotEsq (n-1) (us++[u])
                | otherwise = [u]++us

-- 28 rotDir
rotDir::Int -> [Char] -> [Char]
rotDir n (us)   | n > 0 = rotDir (n-1) ([(last us)] ++ (init us))
                | otherwise = us

-- 29 upper
indice::Char -> Int -> [Char] -> Int
indice _ _ [] = -1
indice c n (p:l) | c == p = n
                 | otherwise = (indice c (n+1) l)

maiuscula::Char -> Char
maiuscula c = ['A'..'Z'] !! (indice c 0 ['a'..'z']) 

upper::[Char] -> [Char]
upper s = [if a `elem` ['a'..'z'] then (maiuscula a) else a | a <- s]

-- 30 titulo
minuscula::Char -> Char
minuscula c = ['A'..'Z'] !! (indice c 0 ['a'..'z']) 

-- titulo::[Char] -> [Char]