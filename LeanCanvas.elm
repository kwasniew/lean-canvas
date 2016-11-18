module LeanCanvas exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, attribute, type_, href, draggable, autofocus, value)
import Html.Events exposing (..)
import Json.Decode as Json
import Dom
import Task
import String
import Dict


type alias Model =
    { uid : Int
    , sections : List Section
    , entryCard : EntryCard
    }


type alias Section =
    { name : String
    , class : String
    , cards : List Card
    }


type alias EntryCard =
    { section : String
    , text : String
    , id : Int
    }


type alias Card =
    { text : String
    , id : Int
    , editing : Bool
    }


initialModel : Model
initialModel =
    { uid = 0
    , sections =
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
    , entryCard = { section = "", text = "", id = 0 }
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
            viewAddCard model.entryCard.text
          else
            text ""
        , div [ class "add-item", onClick (EnableAddCard section) ]
            [ text "Add a card..." ]
        ]


onKeyUp config =
    let
        isCode code =
            case (Dict.get code config) of
                Just msg ->
                    Json.succeed msg

                Nothing ->
                    Json.fail "not the right key code"
    in
        on "keyup" (Json.andThen isCode keyCode)


enter : Int
enter =
    13


escape : Int
escape =
    27


viewAddCard : String -> Html Msg
viewAddCard txt =
    textarea [ Html.Attributes.id "new-card", class "new-card-input", onKeyUp (Dict.fromList [ ( enter, AddCard ), ( escape, DeleteEntryCard ) ]), onInput UpdateEntryCard, autofocus True, value txt ]
        []


viewCard : Card -> Html Msg
viewCard card =
    if card.editing == True then
        textarea
            [ Html.Attributes.id "new-card", class "edit-card-input", autofocus True, value card.text ]
            []
    else
        div [ class "card", draggable "true", onDoubleClick (EnableEditCard card.id) ]
            [ text card.text
            , div [ class "delete-button", onClick (DeleteCard card.id) ]
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
    | EnableEditCard Int
    | DeleteCard Int
    | DeleteEntryCard
    | UpdateEntryCard String
    | NoOp


addCard : Model -> Section -> Section
addCard model section =
    if model.entryCard.section == section.name then
        { section | cards = section.cards ++ [ Card model.entryCard.text model.uid False ] }
    else
        section


deleteCard : Int -> Section -> Section
deleteCard id section =
    { section | cards = List.filter (\card -> card.id /= id) section.cards }


editCard id section =
    { section
        | cards =
            List.map
                (\card ->
                    if card.id == id then
                        { card | editing = True }
                    else
                        card
                )
                section.cards
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        EnableAddCard section ->
            ( { model | entryCard = { section = section.name, text = "", id = 0 } }, Task.attempt (\_ -> NoOp) (Dom.focus "new-card") )

        AddCard ->
            let
                entryCard =
                    model.entryCard
            in
                ( { model
                    | sections = (List.map (addCard model) model.sections)
                    , uid = model.uid + 1
                    , entryCard = { section = entryCard.section, text = "", id = model.uid }
                  }
                , Cmd.none
                )

        EnableEditCard id ->
            ( { model | sections = (List.map (editCard id) model.sections) }, Task.attempt (\_ -> NoOp) (Dom.focus "new-card") )

        DeleteCard id ->
            ( { model | sections = (List.map (deleteCard id) model.sections) }, Cmd.none )

        DeleteEntryCard ->
            ( { model | entryCard = { section = "", text = "", id = 0 } }, Cmd.none )

        UpdateEntryCard str ->
            ( { model | entryCard = { section = model.entryCard.section, text = str, id = model.entryCard.id } }, Cmd.none )
