-- quest 1

mdc :: Int -> Int -> Int
mdc a 0 = a
mdc a b = mdc b (mod a b)

-- quest 2

sort :: [Int] -> [Int] 
sort [] = []
sort [a] = [a]
sort xs = sort (a ++ b) ++ [x] 
    where x = maximum xs
          a = takeWhile (/=x) xs
          b = tail $ dropWhile (/=x) xs 

-- quest 3

subsum u i j = sum [u !! k | k <- [i..j]]

comp (i1, j1, s1) (i2, j2, s2)
    | s1 > s2 = (i1,j1,s1)
    | otherwise = (i2,j2,s2) 

subseq :: [Int] -> [Int]
subseq xs = [xs !! k | k <- [i..j] ]
    where n = length  xs
          t = [ (i, j, subsum xs i j) | i <- [0..n-1], j <- [i, n-1]]
          (i, j, _) = foldl1 comp t

-- quest 4

getPi' a n 
    | a==n = a*a
    | otherwise = a*a + 1.0/(getPi' (a+1) n)

getPi = ( getPi' 1 1000 )**(-1) * 4