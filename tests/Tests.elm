module Tests exposing (..)

import Test exposing (..)
import Expect
import LeanCanvas exposing (Card, Move, reorderCards)


expectReordered : List Int -> Move -> List Int -> Expect.Expectation
expectReordered input move output =
    let
        card id =
            Card "section" (toString id) id False

        cards =
            List.map card input

        expectedReordered =
            List.map card output
    in
        Expect.equal (reorderCards cards move) expectedReordered


all : Test
all =
    describe "Card Reordering"
        [ test "two items - move up" <|
            \() ->
                expectReordered [ 100, 200 ] (Move 100 0 1) [ 200, 100 ]
        , test "three items - move up by one" <|
            \() ->
                expectReordered [ 100, 200, 300 ] (Move 100 0 1) [ 200, 100, 300 ]
        , test "three items - move up by two" <|
            \() ->
                expectReordered [ 100, 200, 300 ] (Move 100 0 2) [ 200, 300, 100 ]
        , test "two items - move down" <|
            \() ->
                expectReordered [ 100, 200 ] (Move 200 1 0) [ 200, 100 ]
        , test "three items - move down by one" <|
            \() ->
                expectReordered [ 100, 200, 300 ] (Move 300 2 1) [ 100, 300, 200 ]
        , test "three items - move down by two" <|
            \() ->
                expectReordered [ 100, 200, 300 ] (Move 300 2 0) [ 300, 100, 200 ]
        ]
