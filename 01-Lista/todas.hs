-- 02 menor entre x y
menord2 x y = min x y

-- 02 menor entre x y z
menord3 x y z = min z (min x y)

-- 03 fatorial
fat x = product [1 .. x]

-- 04 fibonacci
-- INPUT: Inteiro positivo n
-- OUTPUT: n-ésimo termo da sequência de Fibonacci (iniciando em com 0 e 1)
fib 1 = 0 -- condição de parada para a recursão
fib 2 = 1 -- condição de parada para a recursão
fib x = fib (x - 1) + fib (x - 2)

-- 05 elemento
elemento x xs = xs !! x

-- 06 pertence 
pertence x xs = x `elem` xs

-- 07 total
total [] = 0 -- condição de parada para a recursão
total [x] = 1 -- condição de parada para a recursão
total xs = total (tail xs) + 1

-- 08 maior
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