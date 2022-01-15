import Criterion.Main
import Solve (solve, fix, search)
import Matrices
import Check

main :: IO ()
main = defaultMain [
        bench "check-solved" $ whnf check solved_grid,
        bench "search-small" $ whnf search small_grid,
        bench "fix-large" $ whnf fix large_grid,
        bench "fix-small" $ whnf fix small_grid,
        bench "solve-small" $ whnf solve small_grid,
        bench "solve-small-empty" $ whnf solve small_empty_grid,
        bench "solve-large" $ whnf solve large_grid
    ]