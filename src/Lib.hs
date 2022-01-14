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
-- if it is a valid choice at that spot
setRow :: Mark -> Row [Mark] -> Row [Mark]
setRow mark (x:xs) | not (single x) = [mark]:setRow mark xs
    | otherwise = x:setRow mark xs
setRow mark [] = []

-- check if any box in grid has no choices left (invalidates the whole solution grid)
void :: Grid [Mark] -> Bool
void = any (elem [])