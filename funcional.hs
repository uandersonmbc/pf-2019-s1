-- unique xs
unique [] = []
unique [x] = [x]
unique (y : ys) = if y `elem` ys 
    then unique ys else y : unique ys
