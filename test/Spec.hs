import Test.Hspec
import Matrices
import Solve(solve, search, fix, prune)
import Check

main :: IO ()
main = hspec tests

tests :: Spec
tests = do
    describe "to solve 4x4 matrix" $ do
        it "solves 4x4 matrix" $
            solve small_grid `shouldBe` solved_grid
    describe "checks result" $ do
        it "checks if resulting matrix is valid" $
            check solved_grid `shouldBe` True
    describe "search result length" $ do
        it "returns brute forced matrices length" $
            length (search large_grid) `shouldBe` 12
    describe "fixpoint check" $ do
        it "prunes initial input until checkpoint" $
            fix small_grid `shouldBe` fixpoint_small
    describe "prune result" $
        it "prunes grid once" $
            prune small_grid `shouldBe` pruned_small
