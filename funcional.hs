-- quest 2

sort :: [Int] -> [Int] 
sort [] = []
sort [a] = [a]
sort xs = sort (a) ++ [x] 
    where x = maximum xs
          a = [ y | y<-xs, y/=x]