module GameOfLife exposing (..)

import Dict
import Html exposing (Html, program, div, button)
import Html.Events exposing (onClick)
import Set
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, every, second)


type alias Cell =
    ( Int, Int )


type alias Model =
    List Cell


type Msg
    = Next
    | Tick Time


patterns =
    Dict.fromList
        [ ( "small cross", [ ( 24, 26 ), ( 25, 25 ), ( 25, 26 ), ( 25, 27 ), ( 26, 26 ) ] )
        , ( "small exploder", [ ( 24, 26 ), ( 24, 27 ), ( 25, 25 ), ( 25, 26 ), ( 25, 28 ), ( 26, 26 ), ( 26, 27 ) ] )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    every second Tick


initialModel : Model
initialModel =
    Maybe.withDefault [] <| Dict.get "small exploder" patterns


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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


view : Model -> Html Msg
view model =
    let
        myX =
            toString 100
    in
        div []
            [ viewControls
            , viewCells model
            ]


viewControls : Html Msg
viewControls =
    button [ onClick Next ] [ Html.text "next step" ]


viewCells : Model -> Html Msg
viewCells model =
    svg
        [ width "700", height "500", viewBox "0 0 700 500" ]
        (List.map viewCell model)


viewCell : Cell -> Svg Msg
viewCell cell =
    let
        cellX =
            toString <| 10 * Tuple.first cell

        cellY =
            toString <| 10 * Tuple.second cell
    in
        rect [ x cellX, y cellY, width "10", height "10", fill "#0B79CE" ] []


main : Program Never Model Msg
main =
    program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
