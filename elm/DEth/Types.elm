module DEth.Types exposing (..)

import Common.Msg exposing (..)
import Time
import TokenValue exposing (TokenValue)


type alias Model =
    { currentTime : Int
    }


type Msg
    = MsgUp MsgUp
    | Tick Time.Posix


type alias UpdateResult =
    { newModel : Model
    , cmd : Cmd Msg
    , msgUps : List MsgUp
    }


loadingText : String
loadingText =
    "Loading..."


justModelUpdate :
    Model
    -> UpdateResult
justModelUpdate model =
    { newModel = model
    , cmd = Cmd.none
    , msgUps = []
    }
