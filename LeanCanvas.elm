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
    , cards : List String
    }


type alias EntryCard =
    { section : String
    , text : String
    }


initialModel : Model
initialModel =
    { sections =
        [ { name = "key-partners", class = "first-row-column", cards = [] }
        , { name = "key-activities", class = "first-row-column-row", cards = [] }
        , { name = "key-resources", class = "first-row-column-row", cards = [] }
        , { name = "value-proposition", class = "first-row-column", cards = [] }
        , { name = "customer-relationships", class = "first-row-column-row", cards = [] }
        , { name = "channels", class = "first-row-column-row", cards = [] }
        , { name = "customer-segments", class = "first-row-column", cards = [] }
        , { name = "cost-structure", class = "second-row-column", cards = [] }
        , { name = "revenue-streams", class = "second-row-column", cards = [] }
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
                { name = "default-section", class = "", cards = [] }


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
            (List.map viewCard section.cards)
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


viewCard txt =
    div [ class "card", draggable "true" ]
        [ text txt
        , div [ class "delete-button" ]
            [ a [ href "#" ]
                [ text "x" ]
            ]
        ]


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
            [ div [ class "first-row" ]
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


addCard : EntryCard -> Section -> Section
addCard entryCard section =
    if entryCard.section == section.name then
        { section | cards = section.cards ++ [ entryCard.text ] }
    else
        section


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnableAddCard section ->
            ( { model | entryCard = { section = section.name, text = " " } }, Cmd.none )

        AddCard ->
            ( { model | sections = List.map (addCard model.entryCard) model.sections }, Cmd.none )

        UpdateEntryCard str ->
            ( { model | entryCard = { section = model.entryCard.section, text = str } }, Cmd.none )
