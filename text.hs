import System.Environment

func :: [[Char]] -> [([Char]), Int]
func (x:xs) = []

main = do
   args <- getArgs
   contest <- readFile (args !! 0)
   let w = words contest