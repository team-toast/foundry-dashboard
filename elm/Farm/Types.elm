module Farm.Types exposing (..)

import Common.Msg exposing (..)
import Common.Types exposing (..)
import Time
import TokenValue exposing (TokenValue)


type alias Model =
    { userBalanceInfo : Maybe UserBalanceInfo }


type Msg
    = MsgUp MsgUp
    | NoOp


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


type alias UserBalanceInfo =
    { unstaked : TokenValue
    , staked : TokenValue
    , claimableRewardsAtTime : ( TokenValue, Time.Posix )
    }
