module Lista 
( List(..)
, addBeginList
, addEndList
, loadList
, revList
, strList   
) where
	
data List a = Empty | Node a (List a) deriving (Read)

addBeginList x Empty = Node x Empty
addBeginList x ls = Node x ls

addEndList x Empty = Node x Empty
addEndList x (Node y Empty) = Node y (Node x Empty)
addEndList x (Node y ls) = Node y (addEndList x ls)   

loadList :: [t] -> t1 -> (t -> t1 -> t1) -> t1
loadList [] ls _ = ls
loadList (x:xs) ls f = loadList xs (f x ls) f

revList Empty = Empty
revList (Node x ls) = addEndList x (revList ls)

strList :: (Show a) => List a -> String
strList Empty = ""
strList (Node x Empty) = show(x)
strList (Node x ls) = (show x) ++ ", " ++ (strList ls)

instance (Show a) => Show (List a) where	
    	show ls = "{" ++ (strList ls) ++ "}"