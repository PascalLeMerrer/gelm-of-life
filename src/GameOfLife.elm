module GameOfLife exposing (..)

import Debug


type alias Cell =
    ( Int, Int )


type alias Model =
    { width : Int
    , height : Int
    , livingCells : List Cell
    }


init : Int -> Int -> List Cell -> Model
init width height cells =
    { width = width
    , height = height
    , livingCells = cells
    }


update : Model -> Model
update model =
    let
        cellToTest =
            []

        newCells =
            List.filter (\cell -> isAlive (updateCell model cell) cell) model.livingCells
    in
        { model | livingCells = newCells }


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


getNeighbors : Cell -> List Cell
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
        List.filter (\x -> List.member x model.livingCells) neighbors
            |> List.length


spawn : Model -> Cell -> Model
spawn model cell =
    { model | livingCells = cell :: model.livingCells }


kill : Model -> Cell -> Model
kill model cell =
    let
        newCells =
            List.filter (\x -> x /= cell) model.livingCells
    in
        { model | livingCells = newCells }


isAlive : Model -> Cell -> Bool
isAlive model cell =
    List.member cell model.livingCells


isDead : Model -> Cell -> Bool
isDead model cell =
    not <| isAlive model cell
