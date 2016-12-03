module Main exposing (main)

import LeanCanvas
import Navigation exposing (..)
import Html


main : Program Never LeanCanvas.Model LeanCanvas.Msg
main =
    Navigation.program LeanCanvas.locationToMsg
        { view = LeanCanvas.view
        , update = LeanCanvas.update
        , init = LeanCanvas.init
        , subscriptions = LeanCanvas.subscriptions
        }
