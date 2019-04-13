-- fibonacci
-- INPUT: Inteiro positivo n
-- OUTPUT: n-ésimo termo da sequência de Fibonacci (iniciando em com 0 e 1)

fib 1 = 0
fib 2 = 1
fib x = fib (x - 1) + fib (x - 2)