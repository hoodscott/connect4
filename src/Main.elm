module Main exposing (main)

import Browser
import Diagonal exposing (listDiagonalTranspose)
import Html exposing (..)
import Html.Attributes exposing (class, disabled, href, name, rel)
import Html.Events exposing (onClick)
import List.Extra


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- TYPES --


{-| Board is list of Columns (which is a list of Pieces)
-}
type alias Model =
    { board : Board
    , state : GameState
    , players : Players
    }


type alias Board =
    List BoardColumn


type alias BoardColumn =
    List Piece


type Piece
    = Player1Piece
    | Player2Piece
    | Empty


type GameState
    = Player1ToPlay
    | Player2ToPlay
    | GameOver Status


type Status
    = Player1Win
    | Player2Win
    | Draw


type alias Players =
    ( PlayerType, PlayerType )


type PlayerType
    = Human String
    | AI String



-- INIT --


boardSize : { x : number, y : number }
boardSize =
    { x = 7, y = 6 }


initialModel : Model
initialModel =
    { board = List.repeat boardSize.x (List.repeat boardSize.y Empty)
    , state = Player1ToPlay
    , players = ( Human "first player", Human "second player" )
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



-- UPDATE --


type Msg
    = SelectedColumn Int
    | RestartedGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectedColumn columnSelected ->
            case model.state of
                GameOver _ ->
                    ( model, Cmd.none )

                _ ->
                    ( addPieceToCol columnSelected (playerPiece model.state) model
                    , Cmd.none
                    )

        RestartedGame ->
            ( initialModel, Cmd.none )


playerPiece : GameState -> Piece
playerPiece state =
    case state of
        Player1ToPlay ->
            Player1Piece

        Player2ToPlay ->
            Player2Piece

        GameOver _ ->
            Empty


addPieceToCol : Int -> Piece -> Model -> Model
addPieceToCol columnSelected piece model =
    case List.Extra.getAt columnSelected model.board of
        Just newCol ->
            case List.Extra.findIndex (\p -> p == Empty) newCol of
                Just insertIndex ->
                    let
                        newBoard =
                            List.Extra.setAt
                                columnSelected
                                (List.Extra.setAt insertIndex piece newCol)
                                model.board
                    in
                    { model
                        | board = newBoard
                        , state = nextTurn model.state newBoard
                    }

                Nothing ->
                    model

        Nothing ->
            model


nextTurn : GameState -> Board -> GameState
nextTurn state board =
    case checkWin board of
        Just status ->
            GameOver status

        Nothing ->
            case state of
                Player1ToPlay ->
                    Player2ToPlay

                Player2ToPlay ->
                    Player1ToPlay

                GameOver _ ->
                    state


checkWin : Board -> Maybe Status
checkWin board =
    case List.filterMap checkColumn board of
        columnWin :: _ ->
            Just columnWin

        _ ->
            case List.Extra.transpose board |> List.filterMap checkColumn of
                rowWin :: _ ->
                    Just rowWin

                _ ->
                    case listDiagonalTranspose board |> List.filterMap checkColumn of
                        diagAscenWin :: _ ->
                            Just diagAscenWin

                        _ ->
                            case List.map (\list -> List.reverse list) board |> listDiagonalTranspose |> List.filterMap checkColumn of
                                diagDecendWin :: _ ->
                                    Just diagDecendWin

                                _ ->
                                    case List.filter (\column -> List.member Empty column) board of
                                        [] ->
                                            Just Draw

                                        _ ->
                                            Nothing


checkColumn : BoardColumn -> Maybe Status
checkColumn column =
    if List.Extra.isInfixOf [ Player1Piece, Player1Piece, Player1Piece, Player1Piece ] column then
        Just Player1Win

    else if List.Extra.isInfixOf [ Player2Piece, Player2Piece, Player2Piece, Player2Piece ] column then
        Just Player2Win

    else
        Nothing



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW --


view : Model -> Html Msg
view model =
    main_ []
        [ node "link" [ rel "stylesheet", href "style.css" ] []
        , h1 [] [ text "connect 4" ]
        , h2 [] [ viewGameStatus model.players model.state ]
        , div [ class "columns" ] <|
            List.indexedMap (viewColumn model.players (checkGameOver model.state)) model.board
        , hr [] []
        , button [ onClick RestartedGame ] [ text "Restart Game" ]
        ]


checkGameOver : GameState -> Bool
checkGameOver state =
    case state of
        GameOver _ ->
            True

        _ ->
            False


viewGameStatus : Players -> GameState -> Html Msg
viewGameStatus players turn =
    case turn of
        Player1ToPlay ->
            text <| (getName <| Tuple.first players) ++ "'s turn"

        Player2ToPlay ->
            text <| (getName <| Tuple.second players) ++ "'s turn"

        GameOver status ->
            case status of
                Player1Win ->
                    text <| (getName <| Tuple.first players) ++ " wins!"

                Player2Win ->
                    text <| (getName <| Tuple.second players) ++ " wins!"

                Draw ->
                    text "tied game"


getName : PlayerType -> String
getName playerType =
    case playerType of
        Human name ->
            name

        AI name ->
            name


viewColumn : Players -> Bool -> Int -> BoardColumn -> Html Msg
viewColumn players isGameOver columnIndex column =
    let
        canPlaceMore =
            List.member Empty column
    in
    button
        [ class "column"
        , if not isGameOver && canPlaceMore then
            onClick <| SelectedColumn columnIndex

          else
            disabled True
        ]
    <|
        List.reverse <|
            List.map (viewPiece players) column


viewPiece : Players -> Piece -> Html msg
viewPiece players piece =
    let
        pieceStrings =
            case piece of
                Empty ->
                    ( "empty", "Empty" )

                Player1Piece ->
                    ( "p1", (getName <| Tuple.first players) ++ "'s piece" )

                Player2Piece ->
                    ( "p2", (getName <| Tuple.second players) ++ "'s piece" )
    in
    div [ class "piece", class <| Tuple.first pieceStrings ]
        [ span [ class "visually-hidden" ] [ text <| Tuple.second pieceStrings ] ]
