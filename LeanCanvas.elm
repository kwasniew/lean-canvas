module LeanCanvas exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, attribute, type_, href, draggable)
import Html.Events exposing (..)

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


toHeader : String -> String
toHeader dataAttribute =
  (String.split "-" dataAttribute) |> List.map String.toUpper |> String.join " "

viewSection : String -> String -> Html Msg
viewSection cssClass dataAttribute =
    div [ class (cssClass ++ " section"), attribute "data-name" dataAttribute ]
        [ header []
            [ text (toHeader dataAttribute) ]
        , div [ class "scrollable-items" ]
            []
        , div [ class "add-item", onClick EnableAddCard ]
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
            [ viewSection "first-row-column" "key-partners"
            , div [ class "first-row-column" ]
                [ viewSection "first-row-column-row" "key-activities"
                , viewSection "first-row-column-row" "key-resources"
                ]
            , viewSection "first-row-column" "value-proposition"
            , div [ class "first-row-column" ]
                [ viewSection "first-row-column-row" "customer-relationships"
                , viewSection "first-row-column-row" "channels"
                ]
            , viewSection "first-row-column" "customer-segments"
            ]
        , div [ class "second-row" ]
            [ viewSection "second-row-column" "cost-structure"
            , viewSection "second-row-column" "revenue-streams" 
            ]
        ]


type Msg
    = EnableAddCard


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
      EnableAddCard ->
        ( model, Cmd.none )
