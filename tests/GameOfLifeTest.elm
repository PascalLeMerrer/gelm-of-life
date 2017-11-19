module GameOfLifeTest exposing (..)

import Test exposing (..)
import Expect
import GameOfLife exposing (..)


defaultModel =
    [ ( 1, 1 ) ]


deadCells =
    [ ( 0, 0 )
    , ( 1, 0 )
    , ( 2, 0 )
    , ( 0, 1 )
    , ( 2, 1 )
    , ( 0, 2 )
    , ( 1, 2 )
    , ( 2, 2 )
    ]


cellStatusTest : Test
cellStatusTest =
    describe "Cell status should be detected by status functions"
        [ test "tests cell alive status" <|
            \_ ->
                isAlive defaultModel ( 1, 1 )
                    |> Expect.true "Cell should be detected as alive"
        , test "tests cell dead status" <|
            \_ ->
                List.all (isDead defaultModel) deadCells
                    |> Expect.true "Cell should be detected as dead"
        ]


getNeighborCountTest : Test
getNeighborCountTest =
    describe "getNeighborCount should count living cells"
        [ test "unique cell in default model should have no neighbor" <|
            \_ ->
                getNeighborCount defaultModel ( 1, 1 )
                    |> Expect.equal 0
        , test "dead cells in default model should have 1 neighbor" <|
            \_ ->
                List.all (\x -> getNeighborCount defaultModel x == 1) deadCells
                    |> Expect.true "All cells in default model should have 1 neighbor"
        ]


spawnTest : Test
spawnTest =
    describe "spawn should create a new cell"
        [ test "spawn should create a new living cell" <|
            \_ ->
                let
                    newModel =
                        spawn defaultModel ( 0, 1 )
                in
                    isAlive newModel ( 0, 1 )
                        |> Expect.true "Cell should be detected as alive"
        , test "spawn should not kill other living cells" <|
            \_ ->
                let
                    newModel =
                        spawn defaultModel ( 0, 1 )
                in
                    isAlive newModel ( 1, 1 )
                        |> Expect.true "Cell should be detected as alive"
        , test "spawn should increment by one living cell count" <|
            \_ ->
                let
                    newModel =
                        spawn defaultModel ( 0, 1 )
                in
                    getNeighborCount newModel ( 0, 0 )
                        |> Expect.equal 2
        ]


killTest : Test
killTest =
    describe "kill should destroy a cell"
        [ test "kill should remove living cell" <|
            \_ ->
                let
                    newModel =
                        kill defaultModel ( 1, 1 )
                in
                    isDead newModel ( 1, 1 )
                        |> Expect.true "Cell should be detected as dead"
        , test "spawn should decrement by one living cell count" <|
            \_ ->
                let
                    newModel =
                        kill defaultModel ( 1, 1 )
                in
                    getNeighborCount newModel ( 0, 0 )
                        |> Expect.equal 0
        ]


updateCellTest : Test
updateCellTest =
    describe "updateCell should apply game rules to a given cell"
        [ test "isolated cell dies" <|
            \_ ->
                expectCellToBeDeadAfterUpdate defaultModel
        , test "cell with only one neighbor dies" <|
            \_ ->
                expectCellToBeDeadAfterUpdate [ ( 1, 1 ), ( 1, 2 ), ( 4, 4 ) ]
        , test "cell with 2 neighbors survives" <|
            \_ ->
                expectCellToBeAliveAfterUpdate [ ( 1, 0 ), ( 1, 1 ), ( 1, 2 ) ]
        , test "cell with 3 neighbors survives" <|
            \_ ->
                expectCellToBeAliveAfterUpdate [ ( 1, 0 ), ( 0, 1 ), ( 1, 1 ), ( 1, 2 ) ]
        , test "cell with 4 neighbors dies" <|
            \_ ->
                expectCellToBeDeadAfterUpdate [ ( 1, 0 ), ( 0, 1 ), ( 1, 1 ), ( 1, 2 ), ( 2, 1 ) ]
        , test "cell with 5 neighbors dies" <|
            \_ ->
                expectCellToBeDeadAfterUpdate [ ( 1, 0 ), ( 2, 0 ), ( 0, 1 ), ( 1, 1 ), ( 1, 2 ), ( 2, 1 ), ( 4, 4 ) ]
        , test "cell spans when it has 3 neighbors" <|
            \_ ->
                expectCellToBeAliveAfterUpdate [ ( 1, 0 ), ( 0, 1 ), ( 1, 2 ) ]
        ]


expectCellToBeAliveAfterUpdate : Model -> Expect.Expectation
expectCellToBeAliveAfterUpdate startModel =
    let
        newModel =
            updateCell startModel ( 1, 1 )
    in
        isAlive newModel ( 1, 1 )
            |> Expect.true "Cell should be alive"


expectCellToBeDeadAfterUpdate : Model -> Expect.Expectation
expectCellToBeDeadAfterUpdate startModel =
    let
        newModel =
            updateCell startModel ( 1, 1 )
    in
        isDead newModel ( 1, 1 )
            |> Expect.true "Cell should be dead"


updateTest : Test
updateTest =
    describe "update should apply game rules to all cells in the game"
        [ test "isolated cell dies" <|
            \_ ->
                expectTranformTo [ ( 0, 0 ), ( 0, 2 ), ( 2, 0 ), ( 2, 2 ) ]
                    []
        , test "three cells create a new one" <|
            \_ ->
                expectTranformTo [ ( 0, 0 ), ( 1, 0 ), ( 0, 1 ) ]
                    [ ( 0, 0 ), ( 0, 1 ), ( 1, 0 ), ( 1, 1 ) ]
        , test "three neighbors remain unchanged" <|
            \_ ->
                expectTranformTo [ ( 5, 5 ), ( 5, 6 ), ( 6, 5 ), ( 6, 6 ) ]
                    [ ( 5, 5 ), ( 5, 6 ), ( 6, 5 ), ( 6, 6 ) ]
        , test "overpopulation leads to death" <|
            \_ ->
                expectTranformTo [ ( 4, 6 ), ( 5, 5 ), ( 5, 6 ), ( 5, 7 ), ( 6, 6 ) ]
                    [ ( 4, 5 ), ( 4, 6 ), ( 4, 7 ), ( 5, 5 ), ( 5, 7 ), ( 6, 5 ), ( 6, 6 ), ( 6, 7 ) ]
        ]


expectTranformTo : Model -> Model -> Expect.Expectation
expectTranformTo startModel expectedModel =
    let
        newModel =
            update startModel
    in
        Expect.equal newModel expectedModel
