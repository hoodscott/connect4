module AI exposing (getMovesWithMinimax, intMax, intMin)

import List.Extra


{-| Returns a list of indexes of the best possible next move.

`makeMove` takes a game state and returns a list of the next possible game states;
`scoreMove` takes a game state and scores how well the current player is doing;
`maxDepth` determines how many game states in future we will check;
`root` is the current game state.

-}
getMovesWithMinimax : (n -> List n) -> (n -> number) -> Int -> n -> List Int
getMovesWithMinimax makeMove scoreMove maxDepth root =
    let
        maxScore =
            List.Extra.indexedFoldl
                findMaxIndexes
                { max = intMin, indexes = [] }
                childrenScores

        childrenScores =
            List.map (minimax False maxDepth) (makeMove root)

        minimax : Bool -> Int -> n -> number
        minimax isMax depth node =
            let
                children =
                    makeMove node
            in
            if depth == 0 || List.isEmpty children then
                (if isMax then
                    -1

                 else
                    1
                )
                    * scoreMove node

            else
                Maybe.withDefault 0 <|
                    (if isMax then
                        List.maximum

                     else
                        List.minimum
                    )
                    <|
                        (if isMax then
                            intMin

                         else
                            intMax
                        )
                            :: List.map (minimax (not isMax) (depth - 1)) children

        findMaxIndexes index score accum =
            if score > accum.max then
                { accum | max = score, indexes = [ index ] }

            else if score == accum.max then
                { accum | max = score, indexes = index :: accum.indexes }

            else
                accum
    in
    maxScore.indexes


intMin : number
intMin =
    -2 ^ 31


intMax : number
intMax =
    2 ^ 31 - 1
