-- 02 menor entre x y
menord2::Int -> Int -> Int
menord2 x y = min x y

-- 02 menor entre x y z
menord3::Int -> Int -> Int -> Int
menord3 x y z = min z (min x y)

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