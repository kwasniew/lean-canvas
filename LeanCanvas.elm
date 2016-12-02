port module LeanCanvas exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, id, attribute, type_, href, draggable, autofocus, value)
import Html.Events exposing (..)
import Json.Decode as Json
import Dom
import Task
import String
import Dict
import Array
import Maybe


type alias Move =
    { cardId : Int
    , from : Int
    , to : Int
    }


type alias Model =
    { uid : Int
    , cards : List Card
    , entryCard : EntryCard
    , name : String
    , editing : Bool
    , oldName : String
    }


type alias EntryCard =
    { section : String
    , text : String
    , id : Int
    }


type alias Card =
    { section : String
    , text : String
    , id : Int
    , editing : Bool
    }


initialModel : Model
initialModel =
    { uid = 0
    , cards = []
    , entryCard = { section = "", text = "", id = 0 }
    , name = "Business Model Canvas"
    , editing = False
    , oldName = "Business Model Canvas"
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


toHeader : String -> String
toHeader dataAttribute =
    (String.split "-" dataAttribute) |> List.map String.toUpper |> String.join " "


viewSection : Model -> String -> String -> Html Msg
viewSection model section className =
    div [ class (className ++ " section"), attribute "data-name" section ]
        [ header []
            [ text (toHeader section) ]
        , div [ class "scrollable-items" ]
            (List.map (viewCard model.entryCard) (List.filter (\card -> card.section == section) model.cards))
        , if model.entryCard.section == section then
            viewAddCard model.entryCard.text
          else
            text ""
        , div [ class "add-item", onClick (EnableAddCard section) ]
            [ text "Add a card..." ]
        ]


onKeyUp : Dict.Dict Int a -> Attribute a
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


viewCard : EntryCard -> Card -> Html Msg
viewCard entryCard card =
    if card.editing == True then
        textarea
            [ Html.Attributes.id "new-card"
            , class "edit-card-input"
            , onKeyUp (Dict.fromList [ ( enter, ConfirmUpdateCard ), ( escape, (AbortUpdateCard card.id) ) ])
            , onInput UpdateCard
            , autofocus True
            , value entryCard.text
            ]
            []
    else
        div [ class "card", draggable "true", (attribute "data-id" (toString card.id)), onDoubleClick (EnableEditCard card) ]
            [ text card.text
            , div [ class "delete-button", onClick (DeleteCard card.id) ]
                [ a [ href "#" ]
                    [ text "x" ]
                ]
            ]


view : Model -> Html Msg
view model =
    let
        viewSectionWithModel =
            viewSection model
    in
        div [ class "main" ]
            [ h2 [ class "header", onDoubleClick EnableEditName ]
                [ if model.editing == True then
                    input
                        [ type_ "text"
                        , value model.name
                        , Html.Attributes.id "edit-name"
                        , onInput UpdateName
                        , onKeyUp (Dict.fromList [ ( enter, ConfirmEditName ), ( escape, AbortEditName ) ])
                        ]
                        []
                  else
                    text model.name
                ]
            , div [ class "canvas" ]
                [ div [ class "first-row" ]
                    [ viewSectionWithModel "key-partners" "first-row-column"
                    , div [ class "first-row-column" ]
                        [ viewSectionWithModel "key-activities" "first-row-column-row"
                        , viewSectionWithModel "key-resources" "first-row-column-row"
                        ]
                    , viewSectionWithModel "value-proposition" "first-row-column"
                    , div [ class "first-row-column" ]
                        [ viewSectionWithModel "customer-relationships" "first-row-column-row"
                        , viewSectionWithModel "channels" "first-row-column-row"
                        ]
                    , viewSectionWithModel "customer-segments" "first-row-column"
                    ]
                , div [ class "second-row" ]
                    [ viewSectionWithModel "cost-structure" "second-row-column"
                    , viewSectionWithModel "revenue-streams" "second-row-column"
                    ]
                ]
            ]


type Msg
    = EnableAddCard String
    | AddCard
    | EnableEditCard Card
    | ConfirmUpdateCard
    | AbortUpdateCard Int
    | UpdateCard String
    | DeleteCard Int
    | DeleteEntryCard
    | UpdateEntryCard String
    | MoveCard Move
    | EnableEditName
    | UpdateName String
    | ConfirmEditName
    | AbortEditName
    | NoOp


setEditMode : Int -> Bool -> Card -> Card
setEditMode id condition card =
    if card.id == id then
        { card | editing = condition }
    else
        card


saveEditCard : EntryCard -> Card -> Card
saveEditCard entryCard card =
    if card.id == entryCard.id then
        { card | editing = False, text = (String.trimRight entryCard.text) }
    else
        card


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        EnableAddCard section ->
            ( { model
                | entryCard = { section = section, text = "", id = 0 }
                , cards = (List.map (\card -> { card | editing = False }) model.cards)
              }
            , Task.attempt (\_ -> NoOp) (Dom.focus "new-card")
            )

        AddCard ->
            let
                entryCard =
                    model.entryCard

                entryText =
                    String.trimRight entryCard.text
            in
                if String.length entryText > 0 then
                    ( { model
                        | cards = model.cards ++ [ Card model.entryCard.section entryText model.uid False ]
                        , uid = model.uid + 1
                        , entryCard = { section = entryCard.section, text = "", id = model.uid }
                      }
                    , Cmd.none
                    )
                else
                    ( { model | entryCard = { entryCard | text = "" } }, Cmd.none )

        EnableEditCard card ->
            ( { model
                | cards = (List.map (setEditMode card.id True) model.cards)
                , entryCard = { section = "", text = card.text, id = card.id }
              }
            , Task.attempt (\_ -> NoOp) (Dom.focus "new-card")
            )

        ConfirmUpdateCard ->
            ( { model | cards = List.map (saveEditCard model.entryCard) model.cards }, Cmd.none )

        AbortUpdateCard id ->
            ( { model | cards = (List.map (setEditMode id False) model.cards) }, Cmd.none )

        UpdateCard txt ->
            let
                entryCard =
                    model.entryCard
            in
                ( { model | entryCard = { entryCard | text = txt } }, Cmd.none )

        DeleteCard id ->
            ( { model | cards = List.filter (\card -> card.id /= id) model.cards }, Cmd.none )

        DeleteEntryCard ->
            ( { model | entryCard = { section = "", text = "", id = 0 } }, Cmd.none )

        UpdateEntryCard txt ->
            ( { model | entryCard = { section = model.entryCard.section, text = txt, id = model.entryCard.id } }, Cmd.none )

        MoveCard move ->
            ( { model | cards = reorderCards model.cards move }, Cmd.none )

        EnableEditName ->
            ( { model | editing = True }, Task.attempt (\_ -> NoOp) (Dom.focus "edit-name") )

        UpdateName name ->
            ( { model | name = name }, Cmd.none )

        ConfirmEditName ->
            ( { model | editing = False, oldName = model.name }, Cmd.none )

        AbortEditName ->
            ( { model | editing = False, name = model.oldName }, Cmd.none )


reorderCards : List Card -> Move -> List Card
reorderCards cards move =
    case findCardById cards move.cardId of
        Just found ->
            moveCard cards found move.to

        Nothing ->
            cards


findCardById : List Card -> Int -> Maybe Card
findCardById cards id =
    List.head <| (List.filter (\card -> card.id == id) cards)


moveCard : List Card -> Card -> Int -> List Card
moveCard list fromCard toIndex =
    let
        toCardMaybe =
            (Array.get toIndex (Array.fromList list))
    in
        case toCardMaybe of
            Just toCard ->
                List.foldr
                    (\card result ->
                        if card == toCard then
                            toCard :: fromCard :: result
                        else if card == fromCard then
                            result
                        else
                            card :: result
                    )
                    []
                    list

            Nothing ->
                list


port drags : (Move -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    drags MoveCard
