module LeanCanvas exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, attribute, type_, href, draggable)
import Html.Events exposing (..)


type alias Model =
    { sections : List Section
    , addCardSection : String
    }


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
    , addCardSection = ""
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


viewSection : Model -> String -> Section -> Html Msg
viewSection model cssClass section =
    div [ class (cssClass ++ " section"), attribute "data-name" section.name ]
        [ header []
            [ text (toHeader section.name) ]
        , div [ class "scrollable-items" ]
            []
        , if model.addCardSection == section.name then
            viewAddCard
          else
            text ""
        , div [ class "add-item", onClick (EnableAddCard section) ]
            [ text "Add a card..." ]
        ]


viewAddCard : Html Msg
viewAddCard =
    textarea [ class "new-card-input" ]
        []


view : Model -> Html Msg
view model =
    let
        sections =
            model.sections

        getSectionByName =
            getSection sections

        viewSectionWithModel =
            viewSection model
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
            , div [ class "first-row" ]
                [ viewSectionWithModel "first-row-column" (getSectionByName "key-partners")
                , div [ class "first-row-column" ]
                    [ viewSectionWithModel "first-row-column-row" (getSectionByName "key-activities")
                    , viewSectionWithModel "first-row-column-row" (getSectionByName "key-resources")
                    ]
                , viewSectionWithModel "first-row-column" (getSectionByName "value-proposition")
                , div [ class "first-row-column" ]
                    [ viewSectionWithModel "first-row-column-row" (getSectionByName "customer-relationships")
                    , viewSectionWithModel "first-row-column-row" (getSectionByName "channels")
                    ]
                , viewSectionWithModel "first-row-column" (getSectionByName "customer-segments")
                ]
            , div [ class "second-row" ]
                [ viewSectionWithModel "second-row-column" (getSectionByName "cost-structure")
                , viewSectionWithModel "second-row-column" (getSectionByName "revenue-streams")
                ]
            ]


type Msg
    = EnableAddCard Section


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnableAddCard section ->
            ( { model | addCardSection = section.name }, Cmd.none )
