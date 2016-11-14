module LeanCanvas exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, attribute, type_, href, draggable)
import Html.Events exposing (..)


type alias Model =
    { sections : List Section }


type alias Section =
    { name : String
    }


initialModel : Model
initialModel =
    { sections =
        [ { name = "key-partners" }
        , { name = "key-activities" }
        , { name = "key-resources" }
        , { name = "value-proposition" }
        , { name = "customer-relationships" }
        , { name = "channels" }
        , { name = "customer-segments" }
        , { name = "cost-structure" }
        , { name = "revenue-streams" }
        ]
    }


getSection : List Section -> String -> Section
getSection sections name =
    let
        section =
            List.head (List.filter (\section -> section.name == name) sections)
    in
        case section of
            Just s ->
                s

            Nothing ->
                { name = "default-section" }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


toHeader : String -> String
toHeader dataAttribute =
    (String.split "-" dataAttribute) |> List.map String.toUpper |> String.join " "


viewSection : String -> Section -> Html Msg
viewSection cssClass section =
    div [ class (cssClass ++ " section"), attribute "data-name" section.name ]
        [ header []
            [ text (toHeader section.name) ]
        , div [ class "scrollable-items" ]
            []
        , div [ class "add-item", onClick EnableAddCard ]
            [ text "Add a card..." ]
        ]


view : Model -> Html Msg
view model =
    let
        sections =
            model.sections

        getSectionByName =
            getSection sections
    in
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
                [ viewSection "first-row-column" (getSectionByName "key-partners")
                , div [ class "first-row-column" ]
                    [ viewSection "first-row-column-row" (getSectionByName "key-activities")
                    , viewSection "first-row-column-row" (getSectionByName "key-resources")
                    ]
                , viewSection "first-row-column" (getSectionByName "value-proposition")
                , div [ class "first-row-column" ]
                    [ viewSection "first-row-column-row" (getSectionByName "customer-relationships")
                    , viewSection "first-row-column-row" (getSectionByName "channels")
                    ]
                , viewSection "first-row-column" (getSectionByName "customer-segments")
                ]
            , div [ class "second-row" ]
                [ viewSection "second-row-column" (getSectionByName "cost-structure")
                , viewSection "second-row-column" (getSectionByName "revenue-streams")
                ]
            ]


type Msg
    = EnableAddCard


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnableAddCard ->
            ( model, Cmd.none )
