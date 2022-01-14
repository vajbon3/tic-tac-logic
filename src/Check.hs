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
pass grid = noTriple grid && twoPairs grid X && twoPairs grid O && equalMarks grid && unique grid


-- validity conditions

-- max two consecutive X or Os per row and column
twoPairs :: Grid Mark -> Mark -> Bool
twoPairs (row:rows) mark = pairCounter row 0 2 mark
twoPairs [] mark = True 

pairCounter :: Row Mark -> Int -> Int -> Mark -> Bool
pairCounter _ curr max mark | curr > max = False
pairCounter (x:y:xs) curr max mark | x == y && y == mark = pairCounter (y:xs) (curr+1) max mark
    | otherwise = pairCounter (y:xs) curr max mark
pairCounter [_] curr max mark = True
pairCounter [] curr max mark = True


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


-- only two at a time
noTriple :: Grid Mark -> Bool
noTriple = foldr ((&&) . noTripleRow) True

noTripleRow :: Row Mark -> Bool
noTripleRow [] = True
noTripleRow (a:b:c:xs) | a == b && b == c = False
    | otherwise = noTripleRow (b:c:xs)
noTripleRow [_] = True
noTripleRow [_,_] = True

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


