module Messages exposing (..)

import Model exposing (..)
import Http


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
    | Save
    | Update
    | Saved (Result Http.Error String)
    | Fetched (Result Http.Error Model)
    | ChangePage Page
    | Navigate Page
    | NoOp
