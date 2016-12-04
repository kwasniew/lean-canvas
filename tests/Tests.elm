module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import LeanCanvas exposing (Card, Move, reorderCards)


all : Test
all =
    describe "Card Reordering"
        [ test "two items" <|
            \() ->
                let
                    card1 =
                        Card "section" "a" 100 False

                    card2 =
                        Card "section" "b" 101 False

                    cards =
                        [ card1, card2 ]

                    move =
                        Move 100 0 1
                in
                    Expect.equal (reorderCards cards move) [ card2, card1 ]
        , test "String.left" <|
            \() ->
                Expect.equal "a" (String.left 1 "abcdefg")
          -- , test "This test should fail" <|
          --     \() ->
          --         Expect.fail "failed as expected!"
        ]
