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


viewSection : String -> String -> String -> Html Msg
viewSection cssClass dataAttribute headerText =
    div [ class (cssClass ++ " section"), attribute "data-name" dataAttribute ]
        [ header []
            [ text headerText ]
        , div [ class "scrollable-items" ]
            []
        , div [ class "add-item" ]
            [ text "Add a card..." ]
        ]


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
            [ viewSection "first-row-column" "key-partners" "Key Partners"
            , div [ class "first-row-column" ]
                [ viewSection "first-row-column-row" "key-activities" "Key Activities"
                , viewSection "first-row-column-row" "key-resources" "Key Resources"
                ]
            , viewSection "first-row-column" "value-proposition" "Value Proposition"
            , div [ class "first-row-column" ]
                [ viewSection "first-row-column-row" "customer-relationships" "Customer Relationships"
                , viewSection "first-row-column-row" "channels" "Channels"
                ]
            , viewSection "first-row-column" "customer-segments" "Customer Segments"
            ]
        , div [ class "second-row" ]
            [ viewSection "second-row-column" "coost-structure" "Cost Structure"
            , viewSection "second-row-column" "revenue-streams" "Revenue Streams"
            ]
        ]


type Msg
    = Add


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
