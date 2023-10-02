module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra


boardSize : { x : number, y : number }
boardSize =
    { x = 7, y = 6 }


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


{-| board is list of columns (column is list of pieces)
-}
type alias Model =
    { board : Array (Array Piece)
    , state : GameState
    }


type GameState
    = Player1ToPlay
    | Player2ToPlay
    | GameOver Status


type Status
    = Player1Win
    | Player2Win
    | Draw


type Piece
    = Red
    | Yellow
    | Empty


initialModel : Model
initialModel =
    { board = Array.repeat boardSize.x (Array.repeat boardSize.y Empty)
    , state = Player1ToPlay
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


type Msg
    = SelectedColumn Int
    | RestartedGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectedColumn columnSelected ->
            ( addPieceToCol columnSelected (playerPiece model.state) model, Cmd.none )

        RestartedGame ->
            ( initialModel, Cmd.none )


addPieceToCol : Int -> Piece -> Model -> Model
addPieceToCol columnSelected piece model =
    case Array.get columnSelected model.board of
        Just newCol ->
            case List.Extra.findIndex (\p -> p == Empty) (Array.toList newCol) of
                Just insertIndex ->
                    { model
                        | board =
                            Array.set
                                columnSelected
                                (Array.set insertIndex piece newCol)
                                model.board
                        , state = nextTurn model.state
                    }

                Nothing ->
                    model

        Nothing ->
            model


nextTurn : GameState -> GameState
nextTurn state =
    case state of
        Player1ToPlay ->
            Player2ToPlay

        Player2ToPlay ->
            Player1ToPlay

        GameOver _ ->
            state


playerPiece : GameState -> Piece
playerPiece state =
    case state of
        Player1ToPlay ->
            Red

        Player2ToPlay ->
            Yellow

        GameOver _ ->
            Empty


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    let
        _ =
            Debug.log "model" model
    in
    main_ []
        [ node "link" [ rel "stylesheet", href "style.css" ] []
        , h1 [] [ text "connect 4" ]
        , h2 [] [ viewGameStatus model.state ]
        , div [ class "columns" ] <|
            Array.toList <|
                Array.indexedMap viewColumn model.board
        , button [ onClick RestartedGame ] [ text "Restart Game" ]
        ]


viewGameStatus : GameState -> Html Msg
viewGameStatus turn =
    case turn of
        Player1ToPlay ->
            text "player 1's turn"

        Player2ToPlay ->
            text "player 2's turn"

        GameOver status ->
            case status of
                Player1Win ->
                    text "player 1 wins"

                Player2Win ->
                    text "player 2 wins"

                Draw ->
                    text "tied game"


viewColumn : Int -> Array Piece -> Html Msg
viewColumn columnIndex column =
    div [ class "column", onClick <| SelectedColumn columnIndex ] <|
        List.reverse <|
            Array.toList <|
                Array.map viewPiece column


viewPiece : Piece -> Html msg
viewPiece piece =
    case piece of
        Empty ->
            div [ class "piece empty" ] []

        Red ->
            div [ class "piece red" ] []

        Yellow ->
            div [ class "piece yellow" ] []
