module GameOfLife exposing (..)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick)
import Select
import Set
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (..)
import Time exposing (Time, every, second)


type alias Cell =
    ( Int, Int )


type alias Model =
    List Cell


type Msg
    = Next
    | SelectPattern String
    | Tick Time


defaultPattern =
    [ ( 24, 26 ), ( 25, 25 ), ( 25, 26 ), ( 25, 27 ), ( 26, 26 ) ]


patterns : Dict String Model
patterns =
    Dict.fromList
        [ ( "small cross", defaultPattern )
        , ( "small exploder", [ ( 24, 26 ), ( 24, 27 ), ( 25, 25 ), ( 25, 26 ), ( 25, 28 ), ( 26, 26 ), ( 26, 27 ) ] )
        , ( "exploder"
          , [ ( 23, 23 ), ( 23, 24 ), ( 23, 25 ), ( 23, 26 ), ( 23, 27 ), ( 25, 23 ), ( 25, 27 ), ( 27, 23 ), ( 27, 24 ), ( 27, 25 ), ( 27, 26 ), ( 27, 27 ) ]
          )
        , ( "ten cells line", [ ( 20, 25 ), ( 21, 25 ), ( 21, 25 ), ( 22, 25 ), ( 23, 25 ), ( 24, 25 ), ( 25, 25 ), ( 26, 25 ), ( 27, 25 ), ( 28, 25 ), ( 29, 25 ) ] )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    every second Tick


initialModel : Model
initialModel =
    Maybe.withDefault [] <| Dict.get "small cross" patterns


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


view : Model -> Html Msg
view model =
    div []
        [ viewControls
        , viewCells model
        ]


viewControls : Html Msg
viewControls =
    let
        patternNames =
            Dict.keys patterns
    in
        div []
            [ button [ onClick Next ] [ Html.text "Next step" ]
            , Select.from patternNames SelectPattern
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
            toString <| 10 * Tuple.first cell

        cellY =
            toString <| 10 * Tuple.second cell
    in
        rect [ x cellX, y cellY, width "10", height "10", fill "#0B79CE", stroke "#eeeeee" ] []


main : Program Never Model Msg
main =
    program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
