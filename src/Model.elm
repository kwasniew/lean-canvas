module Model exposing (..)


type Page
    = New
    | Existing String


type alias Move =
    { cardId : Int
    , from : Int
    , to : Int
    }


type alias Model =
    { uid : Int
    , cards : List Card
    , entryCard : EntryCard
    , name : String
    , editing : Bool
    , oldName : String
    , page : Page
    , error : Maybe String
    }


type alias EntryCard =
    { section : String
    , text : String
    , id : Int
    }


type alias Card =
    { section : String
    , text : String
    , id : Int
    , editing : Bool
    }


initialModel : Page -> Model
initialModel page =
    { uid = 0
    , cards = []
    , entryCard = { section = "", text = "", id = 0 }
    , name = "Business Model Canvas"
    , editing = False
    , oldName = "Business Model Canvas"
    , page = page
    , error = Nothing
    }
