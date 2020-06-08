module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Attribute, Html, div, span, table, td, text, tr)
import Html.Attributes exposing (attribute, class, style)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


initialModel : Model
initialModel =
    { game = initialGame
    , selected = Nothing
    }


view : Model -> Html Msg
view model =
    div []
        [ viewBoard model.game.board

        -- , div [] [ text <| viewSelected model.selected ]
        -- , div [] [ text <| fromColorToString model.game.turn ]
        ]


viewSelected : Maybe Square -> String
viewSelected selected =
    case selected of
        Just square ->
            fromFileToString square.file ++ fromRankToString square.rank ++ " " ++ fromMaybePieceToString square.piece

        Nothing ->
            ""


fromColorToString : Color -> String
fromColorToString color =
    case color of
        White ->
            "White"

        Black ->
            "Black"


fromRankToString : Rank -> String
fromRankToString rank =
    case rank of
        First ->
            "1"

        Second ->
            "2"

        Third ->
            "3"

        Fourth ->
            "4"

        Fifth ->
            "5"

        Sixth ->
            "6"

        Seventh ->
            "7"

        Eighth ->
            "8"


fromRankToInt : Rank -> Int
fromRankToInt rank =
    case rank of
        First ->
            1

        Second ->
            2

        Third ->
            3

        Fourth ->
            4

        Fifth ->
            5

        Sixth ->
            6

        Seventh ->
            7

        Eighth ->
            8


fromFileToString : File -> String
fromFileToString file =
    case file of
        A ->
            "a"

        B ->
            "b"

        C ->
            "c"

        D ->
            "d"

        E ->
            "e"

        F ->
            "f"

        G ->
            "g"

        H ->
            "h"


fromFileToInt : File -> Int
fromFileToInt file =
    case file of
        A ->
            1

        B ->
            2

        C ->
            3

        D ->
            4

        E ->
            5

        F ->
            6

        G ->
            7

        H ->
            8


update : Msg -> Model -> Model
update msg model =
    case msg of
        Click square ->
            handleClick model square


changeSelected : Model -> Maybe Square -> Model
changeSelected model selected =
    { model | selected = selected }


makeMove : Model -> Square -> Square -> Model
makeMove model fromSq toSq =
    if isValidMove fromSq toSq then
        { model | game = updateGame model.game fromSq toSq, selected = Nothing }

    else
        model


checkPieceColor : Model -> Piece -> Bool
checkPieceColor model piece =
    model.game.turn == piece.color


handleClick : Model -> Square -> Model
handleClick model newSquare =
    case ( model.selected, newSquare.piece ) of
        ( Nothing, Nothing ) ->
            model

        ( Nothing, Just newPiece ) ->
            if checkPieceColor model newPiece then
                changeSelected model <| Just newSquare

            else
                model

        ( Just oldSquare, Nothing ) ->
            case oldSquare.piece of
                Nothing ->
                    model

                Just oldPiece ->
                    if checkPieceColor model oldPiece then
                        makeMove model oldSquare newSquare

                    else
                        model

        ( Just oldSquare, Just newPiece ) ->
            case oldSquare.piece of
                Nothing ->
                    if checkPieceColor model newPiece then
                        changeSelected model <| Just newSquare

                    else
                        model

                Just oldPiece ->
                    if oldSquare == newSquare then
                        changeSelected model Nothing

                    else if checkPieceColor model oldPiece then
                        if checkPieceColor model newPiece then
                            changeSelected model <| Just newSquare

                        else
                            makeMove model oldSquare newSquare

                    else
                        model



-- ( Nothing, Nothing ) ->
--     model
-- ( Just oldSquare, Nothing ) ->
--     if oldSquare.piece /= model.game.turn then
--         model
--     else
--         { model | selected = Just newSquare }
--     { model | game = updateGame model.game oldSquare newSquare, selected = Nothing }
-- ( Nothing, Just newPiece ) ->
--     if newPiece.color /= model.game.turn then
--         model
--     else
--         { model | selected = Just newSquare }
-- ( Just oldSquare, Just newPiece ) ->
--     if newPiece.color /= model.game.turn then
--         model
--     else if oldSquare == newSquare then
--         { model | selected = Nothing }
--     else if isTwoSquaresOccupiedWithSameColor oldSquare newSquare then
--         { model | selected = Just newSquare }
--     else if isValidMove oldSquare newSquare then
--         { model | game = updateGame model.game oldSquare newSquare, selected = Nothing }
--     else
--         model
-- case model.selected of
--     Nothing ->
--         { model | selected = Just newSquare }
--     Just oldSquare ->
--         if oldSquare == newSquare then
--             { model | selected = Nothing }
--         else if isTwoSquaresOccupiedWithSameColor oldSquare newSquare then
--             { model | selected = Just newSquare }
--         else if isValidMove oldSquare newSquare then
--             { model | game = updateGame model.game oldSquare newSquare, selected = Nothing }
--         else
--             model


fromSquareToInt : Square -> Int
fromSquareToInt square =
    let
        rankInt =
            fromRankToInt square.rank

        fileInt =
            fromFileToInt square.file
    in
    8 * (rankInt - 1) + (fileInt - 1)


updateGame : Game -> Square -> Square -> Game
updateGame game oldSquare newSquare =
    { game | board = updateBoard game.board oldSquare newSquare, turn = toggleTurn game.turn }


toggleTurn : Color -> Color
toggleTurn turn =
    case turn of
        White ->
            Black

        Black ->
            White


updateBoard : Board -> Square -> Square -> Board
updateBoard board oldSquare newSquare =
    let
        oldIndex =
            fromSquareToInt oldSquare

        newIndex =
            fromSquareToInt newSquare
    in
    board |> Array.set oldIndex { oldSquare | piece = Nothing } |> Array.set newIndex { newSquare | piece = oldSquare.piece }



-- implement this


isValidMove : Square -> Square -> Bool
isValidMove sq1 sq2 =
    True


isTwoSquaresOccupiedWithSameColor : Square -> Square -> Bool
isTwoSquaresOccupiedWithSameColor sq1 sq2 =
    case ( sq1.piece, sq2.piece ) of
        ( Just piece1, Just piece2 ) ->
            piece1.color == piece2.color

        _ ->
            False


type alias Model =
    { game : Game
    , selected : Maybe Square
    }


type Msg
    = Click Square


type Color
    = White
    | Black


type PieceType
    = Pawn
    | Knight
    | Bishop
    | Rook
    | Queen
    | King


type Rank
    = First
    | Second
    | Third
    | Fourth
    | Fifth
    | Sixth
    | Seventh
    | Eighth


type File
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    | H


type alias Piece =
    { color : Color
    , pieceType : PieceType
    }


type alias Square =
    { file : File
    , rank : Rank
    , piece : Maybe Piece
    }


type alias Board =
    Array Square


type alias Move =
    { from : Square
    , to : Square
    }


type Result
    = Abort
    | NotStarted
    | Ongoing
    | WhiteMate
    | WhiteResign
    | WhiteFlagDown
    | BlackMate
    | BlackResign
    | BlackFlagDown


type alias Game =
    { turn : Color
    , board : Board
    , moves : List Move
    , result : Result
    }


startingBoard : Board
startingBoard =
    Array.fromList
        [ Square A First (Just (Piece White Rook))
        , Square B First (Just (Piece White Knight))
        , Square C First (Just (Piece White Bishop))
        , Square D First (Just (Piece White Queen))
        , Square E First (Just (Piece White King))
        , Square F First (Just (Piece White Bishop))
        , Square G First (Just (Piece White Knight))
        , Square H First (Just (Piece White Rook))
        , Square A Second (Just (Piece White Pawn))
        , Square B Second (Just (Piece White Pawn))
        , Square C Second (Just (Piece White Pawn))
        , Square D Second (Just (Piece White Pawn))
        , Square E Second (Just (Piece White Pawn))
        , Square F Second (Just (Piece White Pawn))
        , Square G Second (Just (Piece White Pawn))
        , Square H Second (Just (Piece White Pawn))
        , Square A Third Nothing
        , Square B Third Nothing
        , Square C Third Nothing
        , Square D Third Nothing
        , Square E Third Nothing
        , Square F Third Nothing
        , Square G Third Nothing
        , Square H Third Nothing
        , Square A Fourth Nothing
        , Square B Fourth Nothing
        , Square C Fourth Nothing
        , Square D Fourth Nothing
        , Square E Fourth Nothing
        , Square F Fourth Nothing
        , Square G Fourth Nothing
        , Square H Fourth Nothing
        , Square A Fifth Nothing
        , Square B Fifth Nothing
        , Square C Fifth Nothing
        , Square D Fifth Nothing
        , Square E Fifth Nothing
        , Square F Fifth Nothing
        , Square G Fifth Nothing
        , Square H Fifth Nothing
        , Square A Sixth Nothing
        , Square B Sixth Nothing
        , Square C Sixth Nothing
        , Square D Sixth Nothing
        , Square E Sixth Nothing
        , Square F Sixth Nothing
        , Square G Sixth Nothing
        , Square H Sixth Nothing
        , Square A Seventh (Just (Piece Black Pawn))
        , Square B Seventh (Just (Piece Black Pawn))
        , Square C Seventh (Just (Piece Black Pawn))
        , Square D Seventh (Just (Piece Black Pawn))
        , Square E Seventh (Just (Piece Black Pawn))
        , Square F Seventh (Just (Piece Black Pawn))
        , Square G Seventh (Just (Piece Black Pawn))
        , Square H Seventh (Just (Piece Black Pawn))
        , Square A Eighth (Just (Piece Black Rook))
        , Square B Eighth (Just (Piece Black Knight))
        , Square C Eighth (Just (Piece Black Bishop))
        , Square D Eighth (Just (Piece Black Queen))
        , Square E Eighth (Just (Piece Black King))
        , Square F Eighth (Just (Piece Black Bishop))
        , Square G Eighth (Just (Piece Black Knight))
        , Square H Eighth (Just (Piece Black Rook))
        ]


fromPieceToString : Piece -> String
fromPieceToString { color, pieceType } =
    case ( color, pieceType ) of
        ( White, Pawn ) ->
            "♙"

        ( White, Knight ) ->
            "♘"

        ( White, Bishop ) ->
            "♗"

        ( White, Rook ) ->
            "♖"

        ( White, Queen ) ->
            "♕"

        ( White, King ) ->
            "♔"

        ( Black, Pawn ) ->
            "♟"

        ( Black, Knight ) ->
            "♞"

        ( Black, Bishop ) ->
            "♝"

        ( Black, Rook ) ->
            "♜"

        ( Black, Queen ) ->
            "♛"

        ( Black, King ) ->
            "♚"


fromMaybePieceToString : Maybe Piece -> String
fromMaybePieceToString maybePiece =
    case maybePiece of
        Just piece ->
            fromPieceToString piece

        Nothing ->
            ""


viewBoard : Board -> Html Msg
viewBoard board =
    table [ class "board" ]
        [ viewBoardRow board 56
        , viewBoardRow board 48
        , viewBoardRow board 40
        , viewBoardRow board 32
        , viewBoardRow board 24
        , viewBoardRow board 16
        , viewBoardRow board 8
        , viewBoardRow board 0
        ]


viewBoardRow : Board -> Int -> Html Msg
viewBoardRow board startIndex =
    let
        endIndex =
            startIndex + 8

        rowElements =
            Array.slice startIndex endIndex board
    in
    tr [] (Array.toList <| Array.map viewBoardSquare rowElements)


viewBoardSquare : Square -> Html Msg
viewBoardSquare square =
    let
        backgroundColor = 
            if modBy 2 (fromFileToInt square.file) == modBy 2 (fromRankToInt square.rank) then
                "#8CA2AD"
            else
                "#DEE3E6"
    in
    td [ 
        onClick (Click square), 
        style "width" "100px", 
        style "height" "100px", 
        style "font-size" "80px", 
        style "border-width" "1px", 
        style "border-style" "solid", 
        style "border-color" "black",
        style "text-align" "center",
        style "vertical-align" "center",
        style "background-color" backgroundColor
    ] [ text <| fromMaybePieceToString square.piece ]


initialGame : Game
initialGame =
    Game White startingBoard [] NotStarted
