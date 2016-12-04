module Main exposing (main)

import LeanCanvas
import Navigation exposing (..)
import Model exposing (Model)
import Messages
import View
import Update


main : Program Never Model Messages.Msg
main =
    Navigation.program LeanCanvas.locationToMsg
        { view = View.view
        , update = Update.update
        , init = LeanCanvas.init
        , subscriptions = LeanCanvas.subscriptions
        }
