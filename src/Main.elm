module Main exposing (main)

import AI
import Browser
import Diagonal exposing (listDiagonalTranspose)
import Html exposing (..)
import Html.Attributes exposing (autocomplete, class, disabled, for, href, id, name, rel, required, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import List.Extra
import Platform exposing (Task)
import Random
import Random.List
import Task


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


type alias Model =
    { game : Game
    , players : Maybe Players
    , formFields : FormFields
    }


type alias Game =
    { board : Board
    , state : State
    }


type alias Board =
    List BoardColumn


type alias BoardColumn =
    List Piece


type Piece
    = Player1Piece
    | Player2Piece
    | Empty


type State
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
    | AI AIType String


type AIType
    = AIRandom
    | AIMinimax


type alias FormFields =
    { player1Name : String
    , player1Type : String
    , player2Name : String
    , player2Type : String
    }



-- INIT --


boardSize : { x : number, y : number }
boardSize =
    { x = 7, y = 6 }


initialModel : Model
initialModel =
    { game =
        { board = createBoard
        , state = Player1ToPlay
        }
    , players = Just ( Human "scott", AI AIMinimax "minimax" )
    , formFields = { player1Name = "", player2Name = "", player1Type = "0", player2Type = "0" }
    }


createBoard : List (List Piece)
createBoard =
    List.repeat boardSize.x (List.repeat boardSize.y Empty)


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



-- UPDATE --


type Msg
    = --game stuff
      SelectedColumn Int
    | StartedAIMove
    | GeneratedAIMove (Maybe Int)
    | SelectedAIMove (Maybe Int)
    | RestartedGame
    | ClearedPlayers
      -- setup form stuff
    | SubmittedPlayerSelectForm
    | ChangedPlayer1Name String
    | ChangedPlayer1Select String
    | ChangedPlayer2Name String
    | ChangedPlayer2Select String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectedColumn columnSelected ->
            case model.game.state of
                GameOver _ ->
                    ( model, Cmd.none )

                _ ->
                    ( { model | game = addPieceToCol model.game columnSelected }
                    , Cmd.none
                    )

        StartedAIMove ->
            case model.players of
                Just players ->
                    ( model
                    , aiSelectMove
                        (currentPlayer model.game.state players)
                        model.game
                    )

                Nothing ->
                    ( model, Cmd.none )

        GeneratedAIMove maybeMove ->
            case maybeMove of
                Just move ->
                    update (SelectedColumn move) model

                _ ->
                    ( model, Cmd.none )

        SelectedAIMove maybeMove ->
            case maybeMove of
                Just move ->
                    update (SelectedColumn (Maybe.withDefault 0 (List.Extra.getAt move <| possibleMoves model.game))) model

                _ ->
                    ( model, Cmd.none )

        RestartedGame ->
            ( { model | game = { board = createBoard, state = Player1ToPlay } }, Cmd.none )

        ClearedPlayers ->
            ( { model | players = Nothing }, Cmd.none )

        SubmittedPlayerSelectForm ->
            if not <| validateForm model.formFields then
                ( model, Cmd.none )

            else
                ( { model
                    | players = createNewPlayers model.formFields
                    , game =
                        { board = createBoard
                        , state = Player1ToPlay
                        }
                  }
                , Cmd.none
                )

        ChangedPlayer1Name name ->
            let
                updFormFields formFields =
                    { formFields | player1Name = name }
            in
            ( { model | formFields = updFormFields model.formFields }, Cmd.none )

        ChangedPlayer1Select controller ->
            let
                updFormFields formFields =
                    { formFields | player1Type = controller }
            in
            ( { model | formFields = updFormFields model.formFields }, Cmd.none )

        ChangedPlayer2Name name ->
            let
                updFormFields formFields =
                    { formFields | player2Name = name }
            in
            ( { model | formFields = updFormFields model.formFields }, Cmd.none )

        ChangedPlayer2Select controller ->
            let
                updFormFields formFields =
                    { formFields | player2Type = controller }
            in
            ( { model | formFields = updFormFields model.formFields }, Cmd.none )


addPieceToCol : Game -> Int -> Game
addPieceToCol game columnSelected =
    let
        piece =
            case game.state of
                Player1ToPlay ->
                    Player1Piece

                Player2ToPlay ->
                    Player2Piece

                GameOver _ ->
                    Empty
    in
    case List.Extra.getAt columnSelected game.board of
        Just newCol ->
            case List.Extra.findIndex (\p -> p == Empty) newCol of
                Just insertIndex ->
                    let
                        newBoard =
                            List.Extra.setAt
                                columnSelected
                                (List.Extra.setAt insertIndex piece newCol)
                                game.board
                    in
                    { board = newBoard
                    , state = nextTurn game.state newBoard
                    }

                Nothing ->
                    game

        Nothing ->
            game


nextTurn : State -> Board -> State
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


currentPlayer : State -> Players -> PlayerType
currentPlayer state players =
    case state of
        Player1ToPlay ->
            Tuple.first players

        Player2ToPlay ->
            Tuple.second players

        GameOver _ ->
            Tuple.first players


aiSelectMove : PlayerType -> Game -> Cmd Msg
aiSelectMove player game =
    let
        send msg =
            Task.succeed msg |> Task.perform identity
    in
    case player of
        AI AIRandom _ ->
            let
                gen : Random.Generator (Maybe Int)
                gen =
                    Random.List.choose (possibleMoves game) |> Random.map (\tuple -> Tuple.first tuple)
            in
            Random.generate GeneratedAIMove gen

        AI AIMinimax _ ->
            send <| SelectedAIMove (AI.getMoveWithMinimax getNextMoves scoreMove 1 game |> Debug.log "minimaxchose")

        Human _ ->
            Cmd.none


scoreMove : Game -> number
scoreMove game =
    case game.state of
        GameOver status ->
            case status of
                Draw ->
                    0

                Player1Win ->
                    20

                Player2Win ->
                    -20

        Player1ToPlay ->
            0

        Player2ToPlay ->
            0


getNextMoves : Game -> List Game
getNextMoves game =
    case game.state of
        GameOver _ ->
            []

        _ ->
            let
                poss =
                    Debug.log "possuible" (possibleMoves game)
            in
            List.map (addPieceToCol game) poss


possibleMoves : Game -> List Int
possibleMoves game =
    List.indexedMap
        (\index column ->
            if List.member Empty column then
                index

            else
                -1
        )
        game.board
        |> List.filter (\index -> index >= 0)


validateForm : FormFields -> Bool
validateForm formFields =
    formFields.player1Name
        /= ""
        && formFields.player2Name
        /= ""
        && formFields.player1Type
        /= ""
        && formFields.player1Type
        /= ""


createNewPlayers : FormFields -> Maybe Players
createNewPlayers formFields =
    let
        createPlayer typeField nameField =
            case typeField formFields of
                "0" ->
                    Just (Human <| nameField formFields)

                "1" ->
                    Just (AI AIRandom <| nameField formFields)

                "2" ->
                    Just (AI AIMinimax <| nameField formFields)

                _ ->
                    Nothing
    in
    case ( createPlayer .player1Type .player1Name, createPlayer .player2Type .player2Name ) of
        ( Just pp1, Just pp2 ) ->
            Just ( pp1, pp2 )

        _ ->
            Nothing



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW --


view : Model -> Html Msg
view model =
    main_ [] <|
        [ node "link" [ rel "stylesheet", href "style.css" ] []
        , h1 [] [ text "connect 4" ]
        ]
            ++ (case model.players of
                    Just players ->
                        [ div [ class "columns" ] <|
                            List.indexedMap
                                (viewColumn players
                                    (checkIsHumanTurn players model.game.state)
                                    (checkGameOver model.game.state)
                                )
                                model.game.board
                        , h2 [] [ viewGameStatus players model.game.state ]
                        , hr [] []
                        , button [ onClick RestartedGame ] [ text "Restart Game" ]
                        , button [ onClick ClearedPlayers ] [ text "Back to player select" ]
                        ]

                    Nothing ->
                        let
                            options =
                                [ "Human", "Random AI", "Minimax AI" ]

                            viewPlayerSelect playerLabel playerName playerNameEvent playerType playerTypeEvent =
                                fieldset []
                                    [ legend [] [ text playerLabel ]
                                    , div []
                                        [ label
                                            [ for <| playerLabel ++ "name" ]
                                            [ text "Name:" ]
                                        , input
                                            [ required True
                                            , onInput playerNameEvent
                                            , value playerName
                                            , autocomplete False
                                            , id <| playerLabel ++ "name"
                                            ]
                                            []
                                        ]
                                    , div []
                                        [ label
                                            [ for <| playerLabel ++ "control" ]
                                            [ text "Controller:" ]
                                        , select
                                            [ required True
                                            , onInput playerTypeEvent
                                            , value playerType
                                            , autocomplete False
                                            , id <| playerLabel ++ "control"
                                            ]
                                          <|
                                            List.indexedMap
                                                (\index choice ->
                                                    option
                                                        [ value <| String.fromInt index ]
                                                        [ text choice ]
                                                )
                                                options
                                        ]
                                    ]
                        in
                        [ form [ onSubmit SubmittedPlayerSelectForm ]
                            [ viewPlayerSelect
                                "Player 1"
                                model.formFields.player1Name
                                ChangedPlayer1Name
                                model.formFields.player1Type
                                ChangedPlayer1Select
                            , viewPlayerSelect
                                "Player 2"
                                model.formFields.player2Name
                                ChangedPlayer2Name
                                model.formFields.player2Type
                                ChangedPlayer2Select
                            , button [ type_ "submit" ] [ text "Start Game" ]
                            ]
                        ]
               )


checkIsHumanTurn : Players -> State -> Bool
checkIsHumanTurn players state =
    let
        tuple =
            case state of
                Player1ToPlay ->
                    Tuple.first

                Player2ToPlay ->
                    Tuple.second

                _ ->
                    Tuple.first
    in
    case tuple players of
        Human _ ->
            True

        AI _ _ ->
            False


checkGameOver : State -> Bool
checkGameOver state =
    case state of
        GameOver _ ->
            True

        _ ->
            False


viewGameStatus : Players -> State -> Html Msg
viewGameStatus players turn =
    let
        addAIButton player =
            case player of
                AI _ _ ->
                    [ button [ onClick StartedAIMove ] [ text "Make AI Move" ] ]

                _ ->
                    []

        playerStatus player =
            span [] <| (text <| (getName <| player) ++ "'s turn") :: addAIButton player
    in
    case turn of
        Player1ToPlay ->
            playerStatus (Tuple.first players)

        Player2ToPlay ->
            playerStatus (Tuple.second players)

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

        AI _ name ->
            name


viewColumn : Players -> Bool -> Bool -> Int -> BoardColumn -> Html Msg
viewColumn players isHumanTurn isGameOver columnIndex column =
    let
        canPlaceMore =
            List.member Empty column
    in
    button
        [ class "column"
        , if not isGameOver && isHumanTurn && canPlaceMore then
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
