module Main where
import Grid
import Solve(solve)

main = do
    input <- getContents
    let solution = solve (parse input)
    print solution


parse :: String -> Grid [Mark]
parse str =
    let rows = tail $ lines str
        grid = map (map marker) rows
        in grid

marker :: Char -> [Mark]
marker 'X' = [X]
marker 'O' = [O]
marker _ = [Empty]

format :: Grid Mark -> String
format (row:rows) = formatRow row  ++ "\n"
format [] = ""

formatRow :: Row Mark -> String
formatRow (x:xs) = show x ++ formatRow xs
formatRow [] = ""
