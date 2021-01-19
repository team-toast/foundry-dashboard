module DerivedEth.Types exposing (..)

import Common.Msg exposing (..)
import Common.Types exposing (DepositOrWithdrawUXModel, JurisdictionCheckStatus, LocationInfo, UserDerivedEthInfo)
import Eth.Types exposing (Address, TxHash)
import Http
import Json.Decode
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
    | DepositClicked TokenValue
    | WithdrawClicked TokenValue
    | UserEthBalanceFetched (Result Http.Error TokenValue)
    | UserDerivedEthBalanceFetched (Result Http.Error TokenValue)
    | DerivedEthRedeemableFetched (Result Http.Error ( TokenValue, TokenValue, TokenValue ))
    | Tick Time.Posix
    | VerifyJurisdictionClicked
    | LocationCheckResult (Result Json.Decode.Error (Result String LocationInfo))
    | ApproveTokenSpend
    | DepositSigned (Result String TxHash)
    | WithdrawSigned (Result String TxHash)
    | FetchUserEthBalance
    | FetchUserDerivedEthBalance


type alias UpdateResult =
    { newModel : Model
    , cmd : Cmd Msg
    , userTxs : List (UserTx.Initiator Msg)
    , msgUps : List MsgUp
    }
