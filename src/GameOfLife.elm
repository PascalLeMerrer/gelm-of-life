module GameOfLife exposing (..)

import Set


type alias Cell =
    ( Int, Int )


type alias Model =
    List Cell


update : Model -> Model
update model =
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
