module Check where
import Grid
import Data.List

checkGrid = [
    [O,O,X,X,O,X,X,O],
    [X,X,O,X,O,O,X,O],
    [O,O,X,O,X,X,O,X],
    [O,X,O,X,O,X,O,X],
    [X,O,X,O,X,O,X,O],
    [O,O,X,O,X,O,X,X],
    [X,X,O,X,O,X,O,O],
    [X,X,O,O,X,O,O,X]
    ]
-- check all grids and return the first correct one
checkGrids :: [Grid Mark] -> Grid Mark
checkGrids (grid:grids) | check grid = grid
    | otherwise = checkGrids grids
checkGrids [] = []

-- check if solution grid is valid
check :: Grid Mark -> Bool
check grid = not (void grid) && passed grid

-- check solution for all validity conditions
passed :: Grid Mark -> Bool
passed grid = pass grid && pass (transpose grid)

pass :: Grid Mark -> Bool
pass grid = twoPairs grid && equalMarks grid && unique grid


-- validity conditions

-- max two consecutive marks per row and column
twoPairs :: Grid Mark -> Bool
twoPairs = foldr (\ row -> (&&) (pairCounter row 0 2)) True

pairCounter :: Row Mark -> Int -> Int -> Bool
pairCounter _ curr max | curr > max = False
    | otherwise = True
pairCounter [] curr max = True
pairCounter (x:y:xs) curr max | x == y = pairCounter xs (curr+1) max
    | otherwise = pairCounter (y:xs) curr max


-- equal amount of x and o's per row and column
equalMarks :: Grid Mark -> Bool
equalMarks = foldr ((&&) . equalMarksRow) True

equalMarksRow :: Row Mark -> Bool
equalMarksRow row = do
    let (x,o) = counter (0,0) row
        in x == o

-- unique rows and columns
unique :: Grid Mark -> Bool
unique grid = length grid == length (nub grid)


-- result grid checking helper functions - analogous to ones in Lib.hs but
-- these ones take Grid Mark instead of Grid [Mark]

-- check if any box in grid is empty (invalidates the solution)
void :: Grid Mark -> Bool
void = any (elem Empty)

-- count number of X,O in a row
counter :: (Int,Int) -> Row Mark -> (Int,Int)
counter (x,o) [] = (x,o)
counter (x,o) (y:ys) | y == X = counter (x+1,o) ys
    | y == O = counter (x,o+1) ys
    | otherwise = counter (x,o) ys


