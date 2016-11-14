module LeanCanvas exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, attribute, type_, href, draggable)


type alias Model =
    { sections : List String
    }


initialModel : Model
initialModel =
    { sections = []
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ div [ class "template card" ]
            [ div [ class "card", draggable "true" ]
                [ text "{{text}}            "
                , div [ class "delete-button" ]
                    [ a [ href "#" ]
                        [ text "x" ]
                    ]
                ]
            ]
        , div [ class "template new-card" ]
            [ input [ type_ "text" ]
                []
            , text "    "
            ]
        , div [ class "first-row" ]
            [ div [ class "first-row-column section", attribute "data-name" "key-partners" ]
                [ header []
                    [ text "Key Partners" ]
                , div [ class "scrollable-items" ]
                    []
                , div [ class "add-item" ]
                    [ text "Add a card..." ]
                ]
            , div [ class "first-row-column" ]
                [ div [ class "first-row-column-row section", attribute "data-name" "key-activities" ]
                    [ header []
                        [ text "Key Activities" ]
                    , div [ class "scrollable-items" ]
                        []
                    , div [ class "add-item" ]
                        [ text "Add a card..." ]
                    ]
                , div [ class "first-row-column-row section", attribute "data-name" "key-resources" ]
                    [ header []
                        [ text "Key Resources" ]
                    , div [ class "scrollable-items" ]
                        []
                    , div [ class "add-item" ]
                        [ text "Add a card..." ]
                    ]
                ]
            , div [ class "first-row-column section", attribute "data-name" "value-proposition" ]
                [ header []
                    [ text "Value Proposition" ]
                , div [ class "scrollable-items" ]
                    []
                , div [ class "add-item" ]
                    [ text "Add a card..." ]
                ]
            , div [ class "first-row-column" ]
                [ div [ class "first-row-column-row section", attribute "data-name" "customer-relationships" ]
                    [ header []
                        [ text "Customer Relationships" ]
                    , div [ class "scrollable-items" ]
                        []
                    , div [ class "add-item" ]
                        [ text "Add a card..." ]
                    ]
                , div [ class "first-row-column-row section", attribute "data-name" "channels" ]
                    [ header []
                        [ text "Channels" ]
                    , div [ class "scrollable-items" ]
                        []
                    , div [ class "add-item" ]
                        [ text "Add a card..." ]
                    ]
                ]
            , div [ class "first-row-column section", attribute "data-name" "customer-segments" ]
                [ header []
                    [ text "Customer Segments" ]
                , div [ class "scrollable-items" ]
                    []
                , div [ class "add-item" ]
                    [ text "Add a card..." ]
                ]
            ]
        , div [ class "second-row" ]
            [ div [ class "second-row-column section", attribute "data-name" "cost-structure" ]
                [ header []
                    [ text "Cost Structure" ]
                , div [ class "scrollable-items" ]
                    []
                , div [ class "add-item" ]
                    [ text "Add a card..." ]
                ]
            , div [ class "second-row-column section", attribute "data-name" "revenue-streams" ]
                [ header []
                    [ text "Revenue Streams" ]
                , div [ class "scrollable-items" ]
                    []
                , div [ class "add-item" ]
                    [ text "Add a card..." ]
                ]
            ]
        ]


type Msg
    = Add


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
