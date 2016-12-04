module ResponseDecoder exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded, custom)
import Model exposing (..)


entryCardDecoder : JD.Decoder EntryCard
entryCardDecoder =
    decode EntryCard
        |> required "section" JD.string
        |> required "text" JD.string
        |> required "id" JD.int


cardDecoder : JD.Decoder Card
cardDecoder =
    decode Card
        |> required "section" JD.string
        |> required "text" JD.string
        |> required "id" JD.int
        |> required "editing" JD.bool


pageDecoder : JD.Decoder Page
pageDecoder =
    decode Existing
        |> required "id" JD.string


modelDecoder : JD.Decoder Model
modelDecoder =
    decode Model
        |> required "uid" JD.int
        |> required "cards" (JD.list cardDecoder)
        |> required "entryCard" entryCardDecoder
        |> required "name" JD.string
        |> required "editing" JD.bool
        |> required "oldName" JD.string
        |> custom (JD.map Existing (JD.field "id" JD.string))
        |> hardcoded Nothing
