module Matrices where

import Grid 

solved_grid = [
        [O,X,X,O],
        [X,O,O,X],
        [O,O,X,X],
        [X,X,O,O]
    ]

fixpoint_small = [
        [[O],[X],[X],[O]],
        [[X],[O],[O],[X]],
        [[O],[O],[X],[X]],
        [[X],[X],[O],[O]]
    ]

pruned_small = [
        [[X,O],[X],[X,O],[O]],
        [[X],[O],[O],[X]],
        [[X,O],[O],[X,O],[X]],
        [[X],[X],[O],[O]]
    ]

large_grid :: [[[Mark]]]
large_grid = [
            [[X,O],[X,O],[X,O],[X],[X,O],[X],[X,O],[X,O]],
            [[X,O],[X],[X,O],[X,O],[X,O],[X,O],[X,O],[X,O]],
            [[X,O],[X,O],[X,O],[X,O],[X],[X],[X,O],[X]],
            [[X,O],[X,O],[O],[X,O],[X,O],[X,O],[O],[X,O]],
            [[X],[X,O],[X,O],[X,O],[X],[X,O],[X,O],[X,O]],
            [[X,O],[X,O],[X,O],[O],[X,O],[X,O],[X],[X]],
            [[X,O],[X],[X,O],[X,O],[X,O],[X,O],[X,O],[X,O]],
            [[X,O],[X,O],[O],[X,O],[X,O],[X,O],[O],[X,O]]]

small_grid :: [[[Mark]]]
small_grid = [
    [[X,O],[X],[X,O],[O]],
    [[X,O],[X,O],[O],[X,O]],
    [[X,O],[O],[X,O],[X,O]],
    [[X],[X],[X,O],[O]]
    ]

small_empty_grid = [
    [[X,O],[X,O],[X,O],[X,O]],
    [[X,O],[X,O],[X,O],[X,O]],
    [[X,O],[X,O],[X,O],[X,O]],
    [[X,O],[X,O],[X,O],[X,O]]
    ]