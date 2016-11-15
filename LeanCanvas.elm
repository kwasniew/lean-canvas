module LeanCanvas exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, attribute, type_, href, draggable)
import Html.Events exposing (..)
import Json.Decode as Json


type alias Model =
    { sections : List Section
    , entryCard : EntryCard
    }


type alias Section =
    { name : String
    , class : String
    }


type alias EntryCard =
    { section : String
    , text : String
    }


initialModel : Model
initialModel =
    { sections =
        [ { name = "key-partners", class = "first-row-column" }
        , { name = "key-activities", class = "first-row-column-row" }
        , { name = "key-resources", class = "first-row-column-row" }
        , { name = "value-proposition", class = "first-row-column" }
        , { name = "customer-relationships", class = "first-row-column-row" }
        , { name = "channels", class = "first-row-column-row" }
        , { name = "customer-segments", class = "first-row-column" }
        , { name = "cost-structure", class = "second-row-column" }
        , { name = "revenue-streams", class = "second-row-column" }
        ]
    , entryCard = { section = "", text = " " }
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
                { name = "default-section", class = "" }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


toHeader : String -> String
toHeader dataAttribute =
    (String.split "-" dataAttribute) |> List.map String.toUpper |> String.join " "


viewSection : Model -> Section -> Html Msg
viewSection model section =
    div [ class (section.class ++ " section"), attribute "data-name" section.name ]
        [ header []
            [ text (toHeader section.name) ]
        , div [ class "scrollable-items" ]
            []
        , if model.entryCard.section == section.name then
            viewAddCard section
          else
            text ""
        , div [ class "add-item", onClick (EnableAddCard section) ]
            [ text "Add a card..." ]
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)


viewAddCard : Section -> Html Msg
viewAddCard section =
    textarea [ class "new-card-input", onEnter AddCard, onInput UpdateEntryCard ]
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
                [ viewSectionWithModel (getSectionByName "key-partners")
                , div [ class "first-row-column" ]
                    [ viewSectionWithModel (getSectionByName "key-activities")
                    , viewSectionWithModel (getSectionByName "key-resources")
                    ]
                , viewSectionWithModel (getSectionByName "value-proposition")
                , div [ class "first-row-column" ]
                    [ viewSectionWithModel (getSectionByName "customer-relationships")
                    , viewSectionWithModel (getSectionByName "channels")
                    ]
                , viewSectionWithModel (getSectionByName "customer-segments")
                ]
            , div [ class "second-row" ]
                [ viewSectionWithModel (getSectionByName "cost-structure")
                , viewSectionWithModel (getSectionByName "revenue-streams")
                ]
            ]


type Msg
    = EnableAddCard Section
    | AddCard
    | UpdateEntryCard String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnableAddCard section ->
            ( { model | entryCard = { section = section.name, text = " " } }, Cmd.none )

        AddCard ->
            ( model, Cmd.none )

        UpdateEntryCard str ->
            ( { model | entryCard = { section = model.entryCard.section, text = str } }, Cmd.none )
