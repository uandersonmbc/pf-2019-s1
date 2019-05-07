import Data.List (minimum, delete)
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

insertion::Int -> [Int] -> [Int]
insertion x [] = [x]
insertion x (y:ys) = if x < y 
                    then x:y:ys else y : insertion x ys
insertionSort::[Int] -> [Int]
insertionSort [] = []
insertionSort (u:us) = insertion u (insertionSort us)

-- Merge Sort
merge::[Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y    = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys

mergeSort::[Int] -> [Int]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort xs = merge (mergeSort (firstHalf xs)) (mergeSort (secondHalf xs))

firstHalf  xs = let { n = length xs } in take (div n 2) xs
secondHalf xs = let { n = length xs } in drop (div n 2) xs

-- Selection Sort

selectionSort :: Ord t => [t] -> [t]
selectionSort [] = []
selectionSort xs = let { x = minimum xs } 
           in  x : selectionSort (delete x xs)