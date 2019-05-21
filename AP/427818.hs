-- NOME: Uanderson Nunes de Lima
-- CURSO: Ciência da Computação
-- MATRICULA: 427818

----------
-- quest 1
----------
digLs 0 f = f
digLs us f = digLs (div us 10) ((mod us 10):f)

lsDig :: Integral t => t -> [t]
lsDig 0 = []
lsDig us = digLs us []

verifPan [] = True
verifPan (u:us) = if elem u us then False else verifPan us

isPanDig :: Integral t => t -> Bool
isPanDig 11 = False
isPanDig n = verifPan (lsDig n)

nfirst [] f = f
nfirst (u:us) f = if elem u us then f else nfirst (us) (f++[u])

lsPanDig :: Integral t => t -> [t]
lsPanDig 11 = []
lsPanDig n = nfirst (lsDig n) []
----------
-- quest 2
----------
delete [] _ = []
delete (u:us) n | u == n = us
                | otherwise = u : delete us n

rmFirst :: Eq a => [a] -> a -> [a]
rmFirst [] _ = []
rmFirst us n = delete us n

sort [] = []
{- sort (us)  = where ma = maximum (u:us)
                me = minimum (u:us)
                a = delete (u:us) ma
                b = delete a me -}

minMaxSort :: Ord t => [t] -> [t]
minMaxSort [] = []
minMaxSort us = sort us

----------
-- quest 3
----------

troca [] _ _ f = f
troca u i j f = troca ([]) (i) (j) ( (a ++ [(head d)]) ++ (init b) ++ [(head b)] ) 
                                        where a = take (i-1) u
                                              b = drop (i-1) u
                                              c = take (j-1) u
                                              d = drop (j-1) u

-- swap :: [a] -> Int -> Int -> [a]
swap [] _ _ = []
swap u i j = troca (u) (u !! i) (u !! j) []

nextPerm :: Ord a => [a] -> [a]
nextPerm     _ = []

----------
-- quest 4
----------

rmChar :: Eq t => [t] -> t -> [t]
rmChar [] _ = []
rmChar (u:us) n | u == n = rmChar us n
                | otherwise = u : rmChar us n

uniqueHehe [] f = f
uniqueHehe (u:us) f = if elem u f then uniqueHehe (us) (f) else uniqueHehe (us) (f++[u])

unique :: Eq t => [t] -> [t]
unique [] = []
unique us = uniqueHehe us []

ocorrencia [] f = f
ocorrencia (u:us) f = ocorrencia (a) (f++[(u, b)])
                            where a = rmChar (u:us) u
                                  b = ( (length (u:us)) - (length a) )

--freqChar :: (Eq a, Num b) => [a] -> [(a, b)]
freqChar [] = []
freqChar us = ocorrencia us []


