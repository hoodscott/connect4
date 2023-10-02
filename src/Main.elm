module Main exposing (main)

import Browser
import Diagonal exposing (listDiagonalTranspose)
import Html exposing (..)
import Html.Attributes exposing (autocomplete, class, disabled, for, href, id, name, rel, required, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import List.Extra
import Random
import Random.List


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
    , players : Maybe Players
    , formFields : FormFields
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
    | AI AIType String


type AIType
    = AIRandom
    | AIUnimplemented


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
    { board = createBoard
    , state = Player1ToPlay
    , players = Just ( Human "scott", AI AIRandom "ai player" )
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
            case model.state of
                GameOver _ ->
                    ( model, Cmd.none )

                _ ->
                    ( addPieceToCol columnSelected (playerPiece model.state) model
                    , Cmd.none
                    )

        StartedAIMove ->
            case model.players of
                Just players ->
                    ( model, aiSelectMove (currentPlayer model.state players) model.board )

                Nothing ->
                    ( model, Cmd.none )

        GeneratedAIMove maybeMove ->
            case maybeMove of
                Just move ->
                    update (SelectedColumn move) model

                _ ->
                    ( model, Cmd.none )

        RestartedGame ->
            ( { model | board = createBoard, state = Player1ToPlay }, Cmd.none )

        ClearedPlayers ->
            ( { model | players = Nothing }, Cmd.none )

        SubmittedPlayerSelectForm ->
            if not <| validateForm model.formFields then
                ( model, Cmd.none )

            else
                ( { model
                    | players = createNewPlayers model.formFields
                    , board = createBoard
                    , state = Player1ToPlay
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


currentPlayer : GameState -> Players -> PlayerType
currentPlayer state players =
    case state of
        Player1ToPlay ->
            Tuple.first players

        Player2ToPlay ->
            Tuple.second players

        GameOver _ ->
            Tuple.first players


aiSelectMove : PlayerType -> Board -> Cmd Msg
aiSelectMove player board =
    case player of
        AI AIRandom _ ->
            let
                possibleMoves =
                    List.indexedMap
                        (\index column ->
                            if List.member Empty column then
                                index

                            else
                                -1
                        )
                        board
                        |> List.filter (\index -> index >= 0)

                gen : Random.Generator (Maybe Int)
                gen =
                    Random.List.choose possibleMoves |> Random.map (\tuple -> Tuple.first tuple)
            in
            Random.generate GeneratedAIMove gen

        AI AIUnimplemented _ ->
            Debug.todo "add another variety of AI"

        Human _ ->
            Cmd.none


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
                                    (checkIsHumanTurn players model.state)
                                    (checkGameOver model.state)
                                )
                                model.board
                        , h2 [] [ viewGameStatus players model.state ]
                        , hr [] []
                        , button [ onClick RestartedGame ] [ text "Restart Game" ]
                        , button [ onClick ClearedPlayers ] [ text "Back to player select" ]
                        ]

                    Nothing ->
                        let
                            options =
                                [ "Human", "Random AI", "Unimplemented AI" ]

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


checkIsHumanTurn : Players -> GameState -> Bool
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


checkGameOver : GameState -> Bool
checkGameOver state =
    case state of
        GameOver _ ->
            True

        _ ->
            False


viewGameStatus : Players -> GameState -> Html Msg
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
