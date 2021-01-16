module DerivedEth.Types exposing (..)

import Common.Msg exposing (..)
import Common.Types exposing (DepositOrWithdrawUXModel, JurisdictionCheckStatus, UserDerivedEthInfo)
import Http
import Time
import TokenValue exposing (TokenValue)
import UserTx
import Wallet exposing (Wallet)


type alias Model =
    { wallet : Wallet
    , userDerivedEthInfo : Maybe UserDerivedEthInfo
    , now : Time.Posix
    , jurisdictionCheckStatus : JurisdictionCheckStatus
    , depositAmount : String
    , withDrawalAmount : String
    }


type Msg
    = MsgUp MsgUp
    | DepositAmountChanged String
    | WithdrawalAmountChanged String
    | DepositClicked
    | WithdrawClicked
    | UserEthBalanceFetched (Result Http.Error TokenValue)
    | UserDerivedEthBalanceFetched (Result Http.Error TokenValue)
    | DerivedEthRedeemableFetched (Result Http.Error ( TokenValue, TokenValue, TokenValue ))
    | Tick Time.Posix


type alias UpdateResult =
    { newModel : Model
    , cmd : Cmd Msg
    , msgUps : List MsgUp
    , userTxs : List (UserTx.Initiator Msg)
    }
