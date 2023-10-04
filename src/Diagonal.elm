module Diagonal exposing (listDiagonalTranspose)

import List.Extra


{-| Turns

```
[[a, [b,  to  [[a] [c, [d]]
  c]  d]]           b]
```

-}
listDiagonalTranspose : List (List a) -> List (List a)
listDiagonalTranspose listOfLists =
    let
        diagonalListLength : Int
        diagonalListLength =
            List.length listOfLists + List.length (Maybe.withDefault [] (List.head listOfLists)) - 1

        diagonalInsertOuter : Int -> List a -> List (List a) -> List (List a)
        diagonalInsertOuter index list accum =
            diagonalInsert 0 index list accum

        diagonalInsert : Int -> Int -> List a -> List (List a) -> List (List a)
        diagonalInsert innerIndex index list accum =
            let
                combinedIndex =
                    innerIndex + index
            in
            case list of
                [] ->
                    accum

                head :: rest ->
                    diagonalInsert (innerIndex + 1)
                        index
                        rest
                    <|
                        List.Extra.setAt combinedIndex
                            (head :: (Maybe.withDefault [] <| List.Extra.getAt combinedIndex accum))
                            accum
    in
    List.Extra.indexedFoldl diagonalInsertOuter (List.repeat diagonalListLength []) listOfLists
