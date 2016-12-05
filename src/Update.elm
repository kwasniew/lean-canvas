module Update exposing (..)

import Model exposing (..)
import Messages exposing (..)
import Task
import Dom
import Http
import BodyEncoder exposing (..)
import Json.Decode as JD
import Navigation exposing (..)
import Array
import ResponseDecoder exposing (..)


setEditMode : Int -> Bool -> Card -> Card
setEditMode id condition card =
    if card.id == id then
        { card | editing = condition }
    else
        card


saveEditCard : EntryCard -> Card -> Card
saveEditCard entryCard card =
    if card.id == entryCard.id then
        { card | editing = False, text = (removeNewLine entryCard.text) }
    else
        card


removeNewLine : String -> String
removeNewLine str =
    String.filter (\c -> c /= '\n') str


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
                    removeNewLine entryCard.text
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
            ( { model | entryCard = { section = model.entryCard.section, text = (removeNewLine txt), id = model.entryCard.id } }, Cmd.none )

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

        Save ->
            ( model, Http.send Saved <| Http.post "/canvas" (Http.stringBody "application/json" (modelToJson model)) (JD.string) )

        Update ->
            case model.page of
                Existing id ->
                    ( model, Http.send Saved <| Http.post ("/canvas/" ++ id) (Http.stringBody "application/json" (modelToJson model)) (JD.string) )

                _ ->
                    ( model, Cmd.none )

        Saved result ->
            case result of
                Ok guid ->
                    ( model, newUrl ("#" ++ guid) )

                Result.Err error ->
                    ( { model | error = Just (toString error) }, Cmd.none )

        ChangePage page ->
            case page of
                New ->
                    ( initialModel New, Cmd.none )

                Existing guid ->
                    ( { model | page = page }, Http.send Fetched <| Http.get ("/canvas/" ++ guid) modelDecoder )

        Navigate page ->
            ( model, newUrl <| pageToHash page )

        Fetched response ->
            case response of
                Ok fetchedModel ->
                    ( fetchedModel, Cmd.none )

                Result.Err error ->
                    let
                        emptyModel =
                            initialModel New
                    in
                        ( { emptyModel | error = Just (toString error) }, Cmd.none )


reorderCards : List Card -> Move -> List Card
reorderCards cards move =
    case findCardById cards move.cardId of
        Just found ->
            moveCard cards found move

        Nothing ->
            cards


findCardById : List Card -> Int -> Maybe Card
findCardById cards id =
    List.head <| (List.filter (\card -> card.id == id) cards)


moveCard : List Card -> Card -> Move -> List Card
moveCard list fromCard move =
    let
        toCardMaybe =
            (Array.get move.to (Array.fromList (List.filter (\card -> card.section == fromCard.section) list)))
    in
        case toCardMaybe of
            Just toCard ->
                List.foldr
                    (\card result ->
                        if card == toCard && (move.from < move.to) then
                            toCard :: fromCard :: result
                        else if card == toCard && (move.from > move.to) then
                            fromCard :: toCard :: result
                        else if card == fromCard then
                            result
                        else
                            card :: result
                    )
                    []
                    list

            Nothing ->
                list


pageToHash : Page -> String
pageToHash page =
    case page of
        New ->
            "#"

        Existing guid ->
            "#" ++ guid
