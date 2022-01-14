module Grid where

data Mark = X | O | Empty
    deriving (Eq)
instance Show Mark where
    show X = "X"
    show O = "O"
    show Empty = "."


type Grid a = Matrix a
type Matrix a = [Row a]
type Row a = [a]  

size :: Grid a -> (Int, Int)
size g@(r:_) = (length g, length r)
size g@[] = (length g,0)

marked :: Mark -> Bool
marked Empty = False
marked _ = True

opposite :: Mark -> Mark
opposite X = O
opposite O = X
opposite Empty = Empty

single :: [Mark] -> Bool
single mark = length mark == 1

simplify :: Grid [Mark] -> Grid Mark
simplify (row:rows) = simplifyRow row : simplify rows
simplify [] = []

simplifyRow (x:xs) = head x : simplifyRow xs
simplifyRow [] = []