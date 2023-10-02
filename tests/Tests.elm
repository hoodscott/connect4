module Tests exposing (..)

import Diagonal exposing (listDiagonalTranspose)
import Expect
import Test exposing (..)


threeByThree : List (List Char)
threeByThree =
    [ [ 'a', 'b', 'c' ], [ 'd', 'e', 'f' ], [ 'h', 'i', 'j' ] ]


threeByThreeAscendingDiag : List (List Char)
threeByThreeAscendingDiag =
    [ [ 'a' ], [ 'd', 'b' ], [ 'h', 'e', 'c' ], [ 'i', 'f' ], [ 'j' ] ]


threeByThreeDescendingDiag : List (List Char)
threeByThreeDescendingDiag =
    [ [ 'c' ], [ 'f', 'b' ], [ 'j', 'e', 'a' ], [ 'i', 'd' ], [ 'h' ] ]


suite : Test
suite =
    describe "Diagonal functions"
        [ test "Gets diagonals for 3x3" <|
            \_ ->
                threeByThree
                    |> listDiagonalTranspose
                    |> Expect.equal threeByThreeAscendingDiag
        , test "Gets descending diagonals for reversed 3x3" <|
            \_ ->
                List.map (\list -> List.reverse list) threeByThree
                    |> listDiagonalTranspose
                    |> Expect.equal threeByThreeDescendingDiag
        ]
