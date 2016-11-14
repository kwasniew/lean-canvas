module Main exposing (main)

import LeanCanvas
import Html

main: Program Never LeanCanvas.Model LeanCanvas.Msg
main =
  Html.program
   { view = LeanCanvas.view
   , update = LeanCanvas.update
   , init = LeanCanvas.init
   , subscriptions = \_ -> Sub.none
  }
