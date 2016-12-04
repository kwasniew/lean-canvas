module BodyEncoder exposing (..)

import Model exposing (..)
import Json.Encode as JE


modelToJson : Model -> String
modelToJson model =
    JE.object
        [ ( "uid", JE.int model.uid )
        , ( "name", JE.string model.name )
        , ( "editing", JE.bool model.editing )
        , ( "oldName", JE.string model.name )
        , ( "cards"
          , JE.list
                (List.map
                    (\card ->
                        JE.object
                            [ ( "section", JE.string card.section )
                            , ( "text", JE.string card.text )
                            , ( "id", JE.int card.id )
                            , ( "editing", JE.bool card.editing )
                            ]
                    )
                    model.cards
                )
          )
        , ( "entryCard"
          , JE.object
                [ ( "section", JE.string model.entryCard.section )
                , ( "text", JE.string model.entryCard.text )
                , ( "id", JE.int model.entryCard.id )
                ]
          )
        ]
        |> JE.encode 4
