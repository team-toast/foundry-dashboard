module Stats.Types exposing (..)

import Common.Msg exposing (..)
import Http
import Eth.Types exposing (Address)

type alias Model =
    { addressMultiSig : Address
    , addressTreasury : Address
    , addressFryToken : Address
    , addressBucketSale : Address
    }


type Msg
    = MsgUp MsgUp


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
