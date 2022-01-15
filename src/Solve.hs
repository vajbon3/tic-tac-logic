module Solve where

import Grid
import Data.List
import Lib
import Check

grid = [
            [[X,O],[X,O],[X,O],[X],[X,O],[X],[X,O],[X,O]],
            [[X,O],[X],[X,O],[X,O],[X,O],[X,O],[X,O],[X,O]],
            [[X,O],[X,O],[X,O],[X,O],[X],[X],[X,O],[X]],
            [[X,O],[X,O],[O],[X,O],[X,O],[X,O],[O],[X,O]],
            [[X],[X,O],[X,O],[X,O],[X],[X,O],[X,O],[X,O]],
            [[X,O],[X,O],[X,O],[O],[X,O],[X,O],[X],[X]],
            [[X,O],[X],[X,O],[X,O],[X,O],[X,O],[X,O],[X,O]],
            [[X,O],[X,O],[O],[X,O],[X,O],[X,O],[O],[X,O]]]

smallGrid = [
    [[X,O],[X],[X,O],[O]],
    [[X,O],[X,O],[O],[X,O]],
    [[X,O],[O],[X,O],[X,O]],
    [[X],[X],[X,O],[O]]]

-- pipeline
solve :: Grid [Mark] -> Grid Mark
solve grid = checkGrids $ search $ fix $ fill grid


-- main pipeline functions

-- fill the empty spaces in the grid with default choice lists - [X,O]
fill :: Grid [Mark] -> Grid [Mark]
fill grid@(x:xs) = fillRow x : fill xs
fill _ = []

fillRow :: Row [Mark] -> Row [Mark]
fillRow (x:xs) | x == [Empty] = [X,O] : fillRow xs
    | otherwise = x : fillRow xs
fillRow [] = []


-- run grid through available techniques and remove impossible grid choices
prune :: Grid [Mark] -> Grid [Mark]
prune grid = setTechniques techniques (transpose (setTechniques techniques (transpose grid)))
     where
         techniques = [
                    avoidingTriples,
                    avoidingTriples3,
                    complete
                    ]


-- recurse prune until fixpoint
fix :: Grid [Mark] -> Grid [Mark]
fix grid =
    let grid' = prune grid
     in if grid == grid' then grid else prune grid'


-- brute force using cartesian product ( only used to turn Grid [Mark] into Grid Mark )
-- not used as a brute force method anymore, too slow
collapse :: Grid [Mark] -> [Grid Mark]
collapse = cp


-- explodes only first box with choices to avoid combinatorial time as opposed to cp :: function
search :: Grid [Mark] -> [Grid Mark]
search m    | Lib.void m = []
            | all (all single) m = collapse m
            | otherwise =
                [g | m' <- expand m, g <- search (prune m') ]


-- iterate through techniques
setTechniques :: [Grid [Mark] -> Grid [Mark]] -> Grid [Mark] -> Grid [Mark]
setTechniques techniques grid = foldl (\ grid technique -> technique grid) grid techniques


-- techniques

-- techniques 1-2
avoidingTriples :: Grid [Mark] -> Grid [Mark]
avoidingTriples = map avoidingTriplesRow

avoidingTriplesRow :: Row [Mark] -> Row [Mark]
-- technique 1
avoidingTriplesRow (arr:[O]:[O]:xs) = delete O arr : avoidingTriplesRow ([O]:[O]:xs)
avoidingTriplesRow (arr:[X]:[X]:xs) = delete X arr : avoidingTriplesRow ([X]:[X]:xs)
avoidingTriplesRow ([O]:[O]:arr:xs) = [O]:[O]: avoidingTriplesRow (delete O arr:xs)
avoidingTriplesRow ([X]:[X]:arr:xs) = [X]:[X]: avoidingTriplesRow (delete X arr:xs)
-- technique 2
avoidingTriplesRow ([O]:arr:[O]:xs) = [O]:delete O arr:avoidingTriplesRow ([O]:xs)
avoidingTriplesRow ([X]:arr:[X]:xs) = [X]:delete X arr:avoidingTriplesRow ([X]:xs)
-- base cases
avoidingTriplesRow (x:xs) = x:avoidingTriplesRow xs
avoidingTriplesRow [] = []


-- technique 3
avoidingTriples3 :: Grid [Mark] -> Grid [Mark]
avoidingTriples3 (row:rows) = avoidingTriples3Row (Lib.counter (0,0,0) row) row : avoidingTriples3 rows
avoidingTriples3 [] = []

avoidingTriples3Row :: (Int,Int,Int) -> Row [Mark] -> Row [Mark]
avoidingTriples3Row (x,o,choices) row 
    | x + 1 == half = replaceMaster indices [X] row
    | o + 1 == half = replaceMaster indices [O] row
    | otherwise = row
    where half = length row `div` 2; indices = elemIndices [X,O] row;

replaceMaster :: [Int] -> [Mark] -> Row [Mark] -> Row [Mark]
replaceMaster [] _ row = row
replaceMaster (idx:idxs) mark row 
    | not (Lib.noTripleRow placeOpposite) = replaceAt idx (opposite mark) row
    | otherwise = replaceMaster idxs mark row
    where
        replacedRow = replaceAt idx mark row

        placeOpposite :: Row [Mark]
        placeOpposite = setRow (opposite mark) replacedRow


-- technique 4
complete :: Grid [Mark] -> Grid [Mark]
complete = map completeRow

completeRow :: Row [Mark] -> Row [Mark]
completeRow row = do
    let (x,o,choices) = Lib.counter (0,0,0) row
        majority = div (length row) 2
        in if x == majority then setRow [O] row
        else if o == majority then setRow [X] row
        else row

