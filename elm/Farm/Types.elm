module Farm.Types exposing (..)

import Common.Msg exposing (..)
import Common.Types exposing (..)

type alias Model =
    {}

type Msg =
    NoOp

type alias UpdateResult =
    { newModel : Model
    , cmd : Cmd Msg
    , msgUps : List MsgUp
    }

justModelUpdate : Model -> UpdateResult
justModelUpdate model =
    { newModel = model
    , cmd = Cmd.none
    , msgUps = []
    }