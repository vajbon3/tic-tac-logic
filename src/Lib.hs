module Lib where
import Grid


-- helper functions

-- cartesian product (calculate all possible grids for brute force )
cp :: Grid [Mark] -> [Grid Mark]
cp [] = [[]]
cp (row:rows) = [y:ys | y <- cpRow row, ys <- cp rows]

-- explode first choice list 
expand :: Grid [Mark] -> [Grid [Mark]]
expand grid =
    [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
    where
       (rows1,row:rows2) = break (any (not . single)) grid
       (row1,cs:row2)    = break (not . single) row


cpRow :: Row [Mark] -> [Row Mark]
cpRow row@(x:xs) = [y:ys | y <- x, ys <- cpRow xs]
cpRow [] = [[]]

-- count number of X,O and possible choice arrays in a row
counter :: (Int,Int,Int) -> Row [Mark] -> (Int,Int,Int)
counter (x,o,choice) [] = (x,o,choice)
counter (x,o,choice) (y:ys) | y == [X] = counter (x+1,o,choice) ys
    | y == [O] = counter (x,o+1,choice) ys
    | otherwise = counter (x,o,choice+1) ys


-- mark unmarked spots in the row with the provided mark, 
setRow :: [Mark] -> Row [Mark] -> Row [Mark]
setRow mark (x:xs) | not (single x) = mark:setRow mark xs
    | otherwise = x:setRow mark xs
setRow mark [] = []

-- set first umarked spot in a row with provided mark 
-- if it is a valid choice at that spot and if it will not create triplets
setFirst :: Mark -> Grid [Mark] -> Row [Mark] -> Row [Mark]
setFirst mark grid (x:xs) | not (single x) && elem mark x && noTriple grid  = [mark]:xs
    | otherwise = x:setFirst mark grid xs
setFirst mark grid [] = []

-- check if any box in grid has no choices left (invalidates the whole solution grid)
void :: Grid [Mark] -> Bool
void = any (elem [])

-- check for triplets
-- only two at a time
noTriple :: Grid [Mark] -> Bool
noTriple = foldr ((&&) . noTripleRow) True

noTripleRow :: Row [Mark] -> Bool
noTripleRow [] = True
noTripleRow (a:b:c:xs) | a == b && b == c = False
    | otherwise = noTripleRow (b:c:xs)
noTripleRow [_] = True
noTripleRow [_,_] = True


replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx element array =
    firstHalf ++ element : drop 1 secondHalf
    where (firstHalf, secondHalf) = splitAt idx array