-------------------------
-- PARTE 1: CONJUNTO (SET)
-------------------------

-- (7.0)

module Aluno (
   Set (..),
   Basic(..),
   get
) where

data Set = Empty | Node Int Set 

class Basic a where
   (/+)  :: a -> a -> a 
   (/-)  :: a -> a -> a
   (//)  :: a -> a -> a  
   (<<)  :: a -> Int -> a
   (/>)  :: a -> Int -> a
   has   :: a -> Int -> Bool
   len   :: a -> Int
   toStr :: a -> String
 
instance Basic Set where
    (/+) _ _ = Empty
    -- (1.0)
    -- implemente uniao
    -- Haskell> let x = Empty << 3 << 2 << 1
    -- Haskell> let y = Empry << 2 << 3 << 4
    -- Haskell> x + y
    -- Haskell> { 3 2 1 4 }

    (<<) _ _ = Empty
    -- (0.5)
    -- implwmwntw insercao em conjunto
    -- Haskell> let x = Empty << 1 << 5 << 3
    -- Haskell> let y = x << 8
    -- Haskell> y << 5
    -- Haskell> { 1 5 3 8 } 
  
    (/>) _ _ = Empty
    -- (0.5)
    -- remocao de elemento
    -- Haskell> let x = Empty << 1 << 4 << 2
    -- Haskell> x /> 4
    -- Haskell> { 1 2 }
 
    (/-)_ _ = Empty
    -- (1.0)
    -- subreacao de conjuntos
    -- Haskell> let x = Empty << 3 << 5 << 8
    -- Haskell> let y = Empty << 3 << 4 << 5
    -- Haskell> x /- y
    -- Haskell> \ { 8 }

    (//) _ _ = Empty
    -- (1.0)
    -- intersecao entre conjuntos
    -- Haskell> let x = Empty << 5 << 4 << 8 << 1
    -- Haskell> let y = Empty << 6 << 4 << 8 << 9
    -- Haskell> x // y
    -- Haskell> { 4 8 } 

    has _ _ = False
    -- (0.5)
    -- testa se conjunto possui elemento
    -- let x = Empty << 3 << 2 << 1 << 6
    -- Haskell> has x 2
    -- Haskell> True
    -- Haskell> has x 7
    -- Haskell> False 

    len _ = 0
    -- (0.25)
    -- determona tamanho do conjunto
    -- Haskell> let x = Empty << 3 << 2 << 1
    -- Haskell> len x
    -- Haskell> 3


    toStr _ = ""
    -- (0.5)
    -- Haskell> transforma conjunto em string
    -- Haskell> toStr (Empty << 3 << 5 << 5 << 4)
    -- Haskell> { 3 5 4 } 
 
get :: String -> Set
get _ = Empty
   -- (1.0)
   -- trasforma string em conjunto
   -- Haskell> get "{ 1 2 3 }"
   -- Haskell> { 1 2 3 }

instance Show Set where
    show _ = ""
    -- (0.25)
    -- teancformma em string
    -- Haskell> toStr (Empty << 3 << 5 << 5 << 4)
    -- Haskell> { 3 5 4 }

instance Eq Set where
    (==) _ _ = True
    -- (0.5) 
    -- compara se dois conjuntos sao iguais
    -- Haskell> let x = Empty << 3 << 2 << 1
    -- Haskell> x == x
    -- Haskell> True

    




