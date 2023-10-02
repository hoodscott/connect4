module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, href, rel)
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
    { board : Board
    , state : GameState
    }


type alias Board =
    List BoardColumn


type alias BoardColumn =
    List Piece


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
    { board = List.repeat boardSize.x (List.repeat boardSize.y Empty)
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
            ( addPieceToCol columnSelected (playerPiece model.state) model
            , Cmd.none
            )

        RestartedGame ->
            ( initialModel, Cmd.none )


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
                    Nothing


checkColumn : List Piece -> Maybe Status
checkColumn column =
    if List.Extra.isInfixOf [ Red, Red, Red, Red ] column then
        Just Player1Win

    else if List.Extra.isInfixOf [ Yellow, Yellow, Yellow, Yellow ] column then
        Just Player2Win

    else
        Nothing


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
            List.indexedMap viewColumn model.board
        , hr [] []
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


viewColumn : Int -> BoardColumn -> Html Msg
viewColumn columnIndex column =
    button [ class "column", onClick <| SelectedColumn columnIndex ] <|
        List.reverse <|
            List.map viewPiece column


viewPiece : Piece -> Html msg
viewPiece piece =
    let
        pieceString =
            case piece of
                Empty ->
                    "empty"

                Red ->
                    "red"

                Yellow ->
                    "yellow"
    in
    div [ class "piece", class pieceString ]
        [ span [ class "visually-hidden" ] [ text pieceString ] ]
