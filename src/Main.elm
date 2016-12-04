module Main exposing (main)

import LeanCanvas
import Navigation exposing (..)
import Model exposing (Model)


main : Program Never Model LeanCanvas.Msg
main =
    Navigation.program LeanCanvas.locationToMsg
        { view = LeanCanvas.view
        , update = LeanCanvas.update
        , init = LeanCanvas.init
        , subscriptions = LeanCanvas.subscriptions
        }
