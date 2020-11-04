module Farm.Types exposing (..)

import Common.Msg exposing (..)
import Common.Types exposing (..)
import Time
import TokenValue exposing (TokenValue)


type alias Model =
    { userBalanceInfo : Maybe UserBalanceInfo
    , depositWithdrawUXModel : DepositWithdrawUXModel
    , now : Time.Posix
    }


type Msg
    = MsgUp MsgUp
    | NoOp
    | UpdateNow Time.Posix
    | StartDeposit
    | StartWithdraw
    | FakeFetchBalanceInfo


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


type alias DepositWithdrawUXModel =
    { inMenu : Maybe DepositOrWithdraw
    }


type DepositOrWithdraw
    = Deposit
    | Withdraw


calcAvailableRewards : UserBalanceInfo -> Time.Posix -> TokenValue
calcAvailableRewards balanceInfo now =
    TokenValue.zero
