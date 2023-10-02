module AI exposing (getMoveWithMinimax)

import List.Extra


getMoveWithMinimax : (n -> List n) -> (n -> number) -> Int -> n -> Maybe Int
getMoveWithMinimax makeMove scoreMove maxDepth root =
    let
        maxScore =
            List.Extra.indexedFoldl
                findMaxIndex
                { max = intMin, index = Nothing }
                childrenScores

        childrenScores =
            List.map (minimax True maxDepth) (makeMove root)

        minimax : Bool -> Int -> n -> number
        minimax isMax depth node =
            let
                children =
                    makeMove node
            in
            if depth == 0 || List.isEmpty children then
                scoreMove node

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

        findMaxIndex index score accum =
            if score > accum.max then
                { accum | max = score, index = Just index }

            else
                accum
    in
    maxScore.index


intMin =
    -2 ^ 31


intMax =
    2 ^ 31 - 1
