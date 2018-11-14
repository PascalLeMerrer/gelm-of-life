module GameOfLife exposing (Cell, Model, Msg(..), defaultPattern, getNeighborCount, getNeighbors, initialModel, isAlive, isDead, kill, main, patterns, spawn, subscriptions, update, updateCell, updateGame, view, viewCell, viewCells, viewControls, viewPatternOption)

import Browser
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (value)
import Html.Events exposing (on, onClick, targetValue)
import Json.Decode as Decode
import Set
import Svg exposing (Svg, rect, svg)
import Svg.Attributes exposing (..)
import Time exposing (Posix, every)


type alias Cell =
    ( Int, Int )


type alias Model =
    List Cell


type Msg
    = SelectPattern String
    | Tick Time.Posix


defaultPattern =
    [ ( 23, 23 ), ( 23, 24 ), ( 23, 25 ), ( 23, 26 ), ( 23, 27 ), ( 25, 23 ), ( 25, 27 ), ( 27, 23 ), ( 27, 24 ), ( 27, 25 ), ( 27, 26 ), ( 27, 27 ) ]


patterns : Dict String Model
patterns =
    Dict.fromList
        [ ( "exploder", defaultPattern )
        , ( "small cross", [ ( 24, 26 ), ( 25, 25 ), ( 25, 26 ), ( 25, 27 ), ( 26, 26 ) ] )
        , ( "small exploder", [ ( 24, 26 ), ( 24, 27 ), ( 25, 25 ), ( 25, 26 ), ( 25, 28 ), ( 26, 26 ), ( 26, 27 ) ] )
        , ( "ten cells line", [ ( 20, 25 ), ( 21, 25 ), ( 21, 25 ), ( 22, 25 ), ( 23, 25 ), ( 24, 25 ), ( 25, 25 ), ( 26, 25 ), ( 27, 25 ), ( 28, 25 ), ( 29, 25 ) ] )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    every 1000 Tick


initialModel : Model
initialModel =
    defaultPattern


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectPattern pattern ->
            let
                selectedPattern =
                    Maybe.withDefault defaultPattern <| Dict.get pattern patterns
            in
            ( selectedPattern, Cmd.none )

        _ ->
            ( updateGame model, Cmd.none )


updateGame : Model -> Model
updateGame model =
    List.concatMap getNeighbors model
        |> List.append model
        |> List.filter (\cell -> isAlive (updateCell model cell) cell)
        |> Set.fromList
        |> Set.toList


updateCell : Model -> Cell -> Model
updateCell model cell =
    let
        neighborCount =
            getNeighborCount model cell

        cellIsAlive =
            isAlive model cell
    in
    if cellIsAlive && (neighborCount < 2) then
        kill model cell

    else if cellIsAlive && (neighborCount > 3) then
        kill model cell

    else if not cellIsAlive && (neighborCount == 3) then
        spawn model cell

    else
        model


getNeighbors : Cell -> Model
getNeighbors cell =
    let
        x =
            Tuple.first cell

        y =
            Tuple.second cell
    in
    [ ( x - 1, y - 1 )
    , ( x, y - 1 )
    , ( x + 1, y - 1 )
    , ( x - 1, y )
    , ( x + 1, y )
    , ( x - 1, y + 1 )
    , ( x, y + 1 )
    , ( x + 1, y + 1 )
    ]


getNeighborCount : Model -> Cell -> Int
getNeighborCount model cell =
    let
        neighbors =
            getNeighbors cell
    in
    List.filter (\x -> List.member x model) neighbors
        |> List.length


spawn : Model -> Cell -> Model
spawn model cell =
    cell
        :: model
        |> Set.fromList
        |> Set.toList


kill : Model -> Cell -> Model
kill model cell =
    List.filter (\x -> x /= cell) model


isAlive : Model -> Cell -> Bool
isAlive model cell =
    List.member cell model


isDead : Model -> Cell -> Bool
isDead model cell =
    not <| isAlive model cell


view : Model -> Browser.Document Msg
view model =
    { title = "Gelm of Life"
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    [ div
        []
        [ viewControls
        , viewCells model
        ]
    ]


viewControls : Html Msg
viewControls =
    let
        patternNames =
            Dict.keys patterns
    in
    div []
        [ select [ on "change" (Decode.map SelectPattern targetValue) ] <|
            List.map viewPatternOption patternNames
        ]


viewPatternOption : String -> Html Msg
viewPatternOption pattern =
    option [ value pattern ] [ text pattern ]


viewCells : Model -> Html Msg
viewCells model =
    svg
        [ width "700", height "500", viewBox "0 0 700 500" ]
        (List.map viewCell model)


viewCell : Cell -> Svg Msg
viewCell cell =
    let
        cellX =
            String.fromInt <| 10 * Tuple.first cell

        cellY =
            String.fromInt <| 10 * Tuple.second cell
    in
    rect [ x cellX, y cellY, width "10", height "10", fill "#0B79CE", stroke "#eeeeee" ] []


init : flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


main : Program Decode.Value Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
