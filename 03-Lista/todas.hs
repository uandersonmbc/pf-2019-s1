data Complex = Complex { real :: Float
                       , img  :: Float
                       }
instance Num Complex where
    (Complex real img) + (Complex real2 img2) = (Complex (real+real2) (img+img2))
    (Complex real img) * (Complex real2 img2) = (Complex (real*real2 - img*img2) (real*real2 + img*img2))
    abs (Complex real img) = (Complex (abs real) (abs img))
    signum (Complex real img) = (Complex (signum real) (signum img))
    fromInteger x = (Complex (fromRational (fromInteger x)) 0)
    negate (Complex real img) = (Complex (negate real) (negate img))

instance Eq Complex where
    (Complex real img) == (Complex real2 img2) = ((comp real real2) && (comp img img2))

instance Fractional Complex where
    (Complex real img) / (Complex real2 img2) = (Complex (real/real2) (img/img2))
    recip (Complex real img) = (Complex (recip real) (recip img))
    fromRational 11.5 =  11.5 --(Complex x 0)

instance Show Complex where
    show (Complex real img) = show real++ "+" ++show img++"i"
        --where sig = if img < 0 then " " else "+"

comp valor valor2 = if(valor == valor2) then True else False


--PILHA
{-data Stack a = Empty | Top a (Stack a)

instance Show a => Show (Stack a) where
    show Empty = " Empty"
    show (Top x Empty) = "Top "++ show x ++ " Empty"
    show (Top x xs) = "Top "++show x ++" ("++show xs++")"

push :: a -> Stack a -> Stack a
push x Empty = (Top x Empty)
push x (Top y ys) = Top x (Top y ys)

pop :: Stack a -> Stack a
pop Empty = Empty
pop (Top x xs) = xs

height :: Stack a -> Int
height Empty = 0
height (Top y ys) = (height ys) + 1

top:: Stack a -> Maybe a
top Empty = Nothing
top (Top x xs) = Just x

isEmpty:: Stack a -> Bool
isEmpty Empty = True
isEmpty _ = False-}


--FILA
{-data Queue a = Empty | Start a (Queue a)

instance Show a => Show (Queue a) where
    show Empty = " Empty"
    show (Start x Empty) = "Start "++ show x ++ " Empty"
    show (Start x xs) = "Start "++ show x ++ "(" ++ show xs ++ ")"

startQueue :: Queue a -> Maybe a
startQueue Empty = Nothing
startQueue (Start x xs) = Just x

endQueue :: Queue a -> Maybe a
endQueue Empty = Nothing
endQueue (Start x Empty) = Just x
endQueue (Start x xs) = endQueue xs

pushQueue :: a -> Queue a -> Queue a
pushQueue x Empty =  Start x Empty
pushQueue x (Start y ys) = Start y (pushQueue x ys)

popQueue :: Queue a -> Queue a
popQueue (Start x xs) = xs

isEmptyQueue :: Queue a -> Bool
isEmptyQueue Empty = True
isEmptyQueue _ = False

lenQueue :: Queue a -> Int
lenQueue Empty = 0
lenQueue (Start x xs) = (lenQueue xs) +1

whileNotEmpty :: (a -> b) -> Queue a -> [b]
whileNotEmpty f Empty = []
whileNotEmpty f (Start x xs) = (f x):(whileNotEmpty f xs)-}

--MATRIZ
type Row = [Float]
data Matrix = Matrix { ncols :: Int
                     , nrows :: Int
                     , rows :: [Row]
                     } deriving (Show)

--instance Show Matrix where
  --  show (Matrix x y []) = "matriz vazia"
    --show (Matrix x y (z:[])) = show z 
--    show (Matrix x y z) = show (head z)++(show (Matrix x y (tail z)))

zeroMatrix :: Int -> Int -> Matrix
zeroMatrix x y = (Matrix x y (prch 0 x y))

oneMatrix :: Int -> Int -> Matrix
oneMatrix x y = (Matrix x y (prch 1 x y))

prch :: Float -> Int -> Int -> [[Float]]
prch x y z = [[x | n<-[1..y]] | m<-[1..z]]

identMatrix :: Int -> Matrix
identMatrix x = (Matrix x x (ident x))

ident x = [[y | m<-[1..x], let y = (zeroOUum m n)] | n<-[1..x]]

zeroOUum m n = if m == n then 1 else 0

sumMatrix :: Matrix -> Matrix -> Matrix
sumMatrix (Matrix x y z) (Matrix x2 y2 z2)
    | ((x==x2)&&(y==y2)) = (Matrix x y (somarr z z2))
    | otherwise = error "matrizes de tamanhos diferentes"

somarr :: [[Float]] -> [[Float]] -> [[Float]]
somarr [] [] = []
somarr x y = (zipWith (+) (head x) (head y)):(somarr (tail x) (tail y))

prodScalar :: Float -> Matrix -> Matrix
prodScalar n (Matrix x y z) = (Matrix x y (porn n z))

porn :: Float -> [[Float]] -> [[Float]]
porn x [] = []
porn x y =  map (*x) (head y):(porn x (tail y))

prodMatrix :: Matrix -> Matrix -> Matrix
prodMatrix (Matrix x y z) (Matrix x2 y2 z2)
    | (y==x2) = (Matrix x2 y (prod z (primeiros z2)))
    | otherwise = error "impossivel fazer a multiplicacao"
--Roubei esse codigo do stackoverflow '-'
primeiros :: [[a]] -> [[a]]
primeiros xs = reverse (helper1 ([], xs))

helper1 :: ([[a]],[[a]]) -> [[a]]
helper1 (hs, []) = hs
helper1 (hs, ls) = helper1 ((f2 . f1 $ ls) : hs, checaNull (f3 . f1 $ ls))
  where f1 y = map (splitAt 1) y
        f2 y = concat $ map fst y
        f3 y = map snd y
        checaNull [] = []
        checaNull ls@(x:xs) = if not (null x) then ls else checaNull xs

-- '-'
prod :: [[Float]] -> [[Float]] -> [[Float]] --Ta errado
prod [] _ = []
prod x y = (listaa x y):(prod (tail x) y)

listaa :: [[Float]] -> [[Float]] -> [Float]
listaa _ [] = []
listaa x y = sum(zipWith (*)(head x) (head y)):(listaa  x (tail y))

listToMatrix :: [Row] -> Matrix
listToMatrix x = (Matrix (length (head x)) (length x) x)

--DATE
-- data Day = Int
-- data Month = Janeiro | Fevereiro | Março | Abril | Maio | Junho | Julho | Agosto | Setembro | Outubro | Novembro | Dezembro deriving(Eq, Ord, Show, Enum, Bounded)
-- data Year = Int
-- data Date = Date { Day
--                  , Month
--                  , Year
--                  } deriving (Eq, Ord, Show)

-- srtToDate srt = (Date d (m-1) a)  where
--     d = read (w !! 0) :: Int
--     m = toEnum (read (w !! 1) :: Month) 
--     a = read (w !! 2) :: Int
--     w = words str2
--     srt2 = [f ch | ch <- srt]
--     f c = if c == "/" then " " else c

--sortListDates :: [Date] -> [Date]
--sortListDates (Date x y z) =

--PESSOA
data Pessoa = Pessoa { nome :: String
                     , idade :: Int
                     , salario :: Float
                     } deriving (Ord, Eq)
data Criterio = ByNome | ByIdade | BySalario deriving(Eq)

instance Show Pessoa where
    show (Pessoa x y z) = "Nome: "++x++" Idade: "++show y++" Salario: "++show z

sortListPessoa :: [Pessoa] -> Criterio -> [Pessoa]
sortListPessoa x y
    | y == ByNome = quicksortPessoa x 1
    | y == ByIdade = quicksortPessoa x 2
    | y == BySalario = quicksortPessoa x 3

quicksortPessoa :: [Pessoa] -> Int -> [Pessoa]
quicksortPessoa [] d = []
quicksortPessoa (x:xs) d =
    let smallerSorted = quicksortPessoa [a | a <- xs, (menor a x d)] d  
        biggerSorted = quicksortPessoa [a | a <- xs, (not (menor a x d))] d 
    in  smallerSorted ++ [x] ++ biggerSorted

menor :: Pessoa -> Pessoa -> Int -> Bool
menor (Pessoa x y z) (Pessoa x2 y2 z2) d
    | d == 1 = if x <= x2 then True else False
    | d == 2 = if y <=y2 then True else False
    | d == 3 = if z <=z2 then True else False

--OList
data OList a = Empty | Node a (OList a)

instance Show a => Show (OList a) where
    show Empty = " Empty"
    show (Node x y) = (show x) ++ " >>> " ++ (show y)

infixr 5 >>>
(>>>) :: (Ord a) => a -> OList a -> OList a
a >>> Empty = Node a Empty
a >>> (Node x y)
    | a <= x = (Node a (Node x y))
    | a > x = (Node x (a >>> y))

hasKey :: (Ord a) => a -> OList a -> Bool
hasKey a Empty = False
hasKey a (Node x y)
    | a < x = False
    | a == x = True
    | a > x = hasKey a y 

remKey :: (Ord a) => a -> OList a -> OList a
remKey a Empty = Empty
remKey a (Node x y)
    | a == x = y
    | a < x = (Node x y)
    | a > x = (Node x (remKey a y))

key :: Int -> OList a -> Maybe a
key 0 _ = Nothing
key n Empty = Nothing
key n (Node x y)
    | n == 1 = Just x
    | n > 1 = key (n-1) y