module LeanCanvas exposing (..)

import Html exposing (..)


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


view : Model -> Html Msg
view model =
    div [] [ text "Lean Canvas" ]


type Msg
    = Add


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
