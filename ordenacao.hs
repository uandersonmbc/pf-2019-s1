-- Quick Sort
quicksort::[Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort small ++ (x : quicksort large)
    where small = [y | y <- xs, y <= x]
          large = [y | y <- xs, y > x]

-- Bubble Sort
sort::[Int] -> [Int] -> [Int]
sort [] f = f
sort [a] f = [a]++f
sort us f = sort (a ++ b) ([x]++f)
    where x = maximum us
          a = takeWhile (/=x) us
          b = tail $ dropWhile (/=x) us 
bubbleSort::[Int] -> [Int]
bubbleSort us = sort us []

-- Insertion Sort

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys) = if x < y 
                    then x:y:ys else y : insert x ys
insertionSort::[Int] -> [Int]
insertionSort [] = []
insertionSort (u:us) = insert u (insertionSort us)