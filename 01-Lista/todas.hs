-- 02 menor entre x y
menor x y = min x y

-- 02 menor entre x y z
menor x y z = min z (min x y)

-- 03 fatorial
fat x = product [1 .. x]

-- 04 fibonacci
-- INPUT: Inteiro positivo n
-- OUTPUT: n-ésimo termo da sequência de Fibonacci (iniciando em com 0 e 1)
fib 1 = 0
fib 2 = 1
fib x = fib (x - 1) + fib (x - 2)

