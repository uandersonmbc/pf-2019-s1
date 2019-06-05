import System.Environment
make::[Char] -> Double -> Double -> Double
make s x y | s == "add" = x+y
           | s == "sub" = x-y
           | s == "mul" = x*y
           | s == "div" = x/y

main = do
   args <- getArgs
   let n = length args
   if(n == 3) then do $
               let cmd = args !! 0
                   x = (read (args !! 1))::Double
                   y = (read (args !! 2))::Double
                   r = make cmd x y
               putStrLn (show r)
   else error "Fail tests"