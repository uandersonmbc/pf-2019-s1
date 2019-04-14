-- 02 menor entre x y
menord2 x y = min x y

-- 02 menor entre x y z
menord3 x y z = min z (min x y)

-- 03 fatorial
fat x = product [1 .. x]

-- 04 fibonacci
-- INPUT: Inteiro positivo n
-- OUTPUT: n-ésimo termo da sequência de Fibonacci (iniciando em com 0 e 1)
fib 1 = 0
fib 2 = 1
fib x = fib (x - 1) + fib (x - 2)

-- 05 elemento
elemento x xs = xs !! x

-- 06 pertence 
pertence x xs = x `elem` xs

-- 07 total
total [] = 0
total [x] = 1
total xs = total (tail xs) + 1

-- 08 frequencia
