module DerivedEth.Types exposing (..)

import Common.Msg exposing (..)
import Common.Types exposing (DepositOrWithdrawUXModel, JurisdictionCheckStatus, UserDerivedEthInfo)
import Time
import TokenValue exposing (TokenValue)
import UserTx
import Wallet exposing (Wallet)


type alias Model =
    { wallet : Wallet
    , userDerivedEthInfo : Maybe UserDerivedEthInfo
    , depositWithdrawUXModel : DepositOrWithdrawUXModel
    , now : Time.Posix
    , jurisdictionCheckStatus : JurisdictionCheckStatus
    }


type Msg
    = MsgUp MsgUp
    | Tick Time.Posix


type alias UpdateResult =
    { newModel : Model
    , cmd : Cmd Msg
    , msgUps : List MsgUp
    , userTxs : List (UserTx.Initiator Msg)
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
    , userTxs = []
    }
