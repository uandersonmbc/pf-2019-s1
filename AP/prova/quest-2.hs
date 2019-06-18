----------------------------------
-- PAETE 2: PROCESAMENTO DE TEXTO
----------------------------------

--(6.0)

import System.Environment 

-- (0.5)
-- retorna substring de str que inicia
-- na posicao i e possui m caracteres
sub str i m = drop i $ take (i+m) str

-- (1.0)
-- procura na steing s a string 
-- t a partir da posição j. 
-- Retiena o indice onde t inicia
-- e -1 se t não pertence a s

teste :: String -> Int -> Int -> String
teste ss i t = (sub ss 0 t)

myFind [] _ _ = -1
myFind (s:ss) t i = if ( (teste (s:ss) 0 (length t)) == t) then i else myFind (ss) (t) (i+1)

find :: String -> String -> Int -> Int
find s t j = myFind (drop j s) (t) (0)

-- (1.0)
-- substitui em s primeira aparicao
-- de w em s por t (a partir da posicao 
-- j caso seja possivel. Do dontearui 
-- retorna o proptia s 

myReplaceOne [] _ _ f = f 
myReplaceOne (s:ss) w t f = if ( (teste (s:ss) 0 (length w)) == w) then (f ++ t ++ (drop (length w) (s:ss))) else myReplaceOne (ss) (w) (t) (f ++ [s])

replaceOne :: String -> String -> String -> Int -> String
replaceOne s w t j = (take j s) ++ myReplaceOne (drop j s) (w) (t) ("")

-- (2.0)
-- substitui en s todas as 
-- aparicoes de w por t, 
-- a partir da posicao j


myReplace [] _ _ f = f 
myReplace (s:ss) w t f = if ( (teste (s:ss) 0 (length w)) == w) then myReplace (drop (length w) (s:ss)) (w) (t) (f++t) else myReplace (ss) (w) (t) (f ++ [s])

replace :: String -> String -> String -> Int -> String 
replace s w t j = (take j s) ++ myReplace (drop j s) (w) (t) ("")

-- main = do
--     args <- getArgs
--     name <- getProgName

    -- (1.5)

    -- Lê arqivo via linha de comando
    -- e constroi outro arquivo com 
    -- sequencia substituisa. 
    -- Por exemplo, no prompt, 
     --
     -- >> subst file1 "gato cinza" "cachorro" file2
     --
     -- f1le2 possui o mesmo 
     -- onteudo de file1 exceto 
     -- pot toda swquencia "gato cinza"
     -- ter sido sustituida por 
     -- "cachirro". subst é o nome 
     -- do programa que
     -- foi criado









