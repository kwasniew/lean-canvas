module View exposing (..)

import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, id, attribute, type_, href, draggable, autofocus, value)
import Html.Events exposing (..)
import Dict
import Messages exposing (..)
import Json.Decode as JD


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
                    JD.succeed msg

                Nothing ->
                    JD.fail "not the right key code"
    in
        on "keyup" (JD.andThen isCode keyCode)


enter : Int
enter =
    13


escape : Int
escape =
    27


viewAddCard : String -> Html Msg
viewAddCard txt =
    textarea
        [ Html.Attributes.id "new-card"
        , class "new-card-input"
        , onBlur DeleteEntryCard
        , onKeyUp (Dict.fromList [ ( enter, AddCard ), ( escape, DeleteEntryCard ) ])
        , onInput UpdateEntryCard
        , autofocus True
        , value txt
        ]
        []


viewCard : EntryCard -> Card -> Html Msg
viewCard entryCard card =
    if card.editing == True then
        textarea
            [ Html.Attributes.id "new-card"
            , class "edit-card-input"
            , onBlur (AbortUpdateCard card.id)
            , onKeyUp (Dict.fromList [ ( enter, ConfirmUpdateCard ), ( escape, (AbortUpdateCard card.id) ) ])
            , onInput UpdateCard
            , autofocus True
            , value entryCard.text
            ]
            []
    else
        div [ class "card", draggable "true", (attribute "data-id" (toString card.id)), onDoubleClick (EnableEditCard card) ]
            [ text card.text
            , div [ class "delete-button" ]
                [ span [ onClick (DeleteCard card.id) ]
                    [ text "delete" ]
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
                        , onBlur AbortEditName
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
            , div [ class "action-area" ]
                [ button
                    [ class "save-button action-button"
                    , if model.page == New then
                        onClick Save
                      else
                        onClick Update
                    ]
                    [ text
                        (if model.page == New then
                            "save"
                         else
                            "update"
                        )
                    ]
                , button
                    [ onClick (Navigate New)
                    , class "new-button action-button"
                    ]
                    [ text "new canvas" ]
                ]
            , div []
                [ text (textError model.error)
                ]
            ]


textError : Maybe String -> String
textError modelError =
    case modelError of
        Just error ->
            error

        _ ->
            ""
