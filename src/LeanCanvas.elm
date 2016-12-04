port module LeanCanvas exposing (..)

import Http
import Navigation exposing (..)
import Model exposing (..)
import ResponseDecoder exposing (..)
import Messages exposing (..)


init : Location -> ( Model, Cmd Msg )
init location =
    let
        page =
            hashToPage location.hash

        model =
            initialModel page
    in
        case page of
            New ->
                ( model, Cmd.none )

            Existing guid ->
                ( model, Http.send Fetched <| Http.get ("/canvas/" ++ guid) ResponseDecoder.modelDecoder )


port drags : (Move -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    drags MoveCard


hashToPage : String -> Page
hashToPage hash =
    case hash of
        "" ->
            New

        val ->
            Existing (String.dropLeft 1 val)


locationToMsg : Location -> Msg
locationToMsg location =
    location.hash
        |> hashToPage
        |> ChangePage
