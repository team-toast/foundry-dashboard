module Farm.Types exposing (..)

import Common.Msg exposing (..)
import Common.Types exposing (..)
import Time
import TokenValue exposing (TokenValue)


type alias Model =
    { timedUserStakingInfo : Maybe TimedUserStakingInfo
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


type alias TimedUserStakingInfo =
    { userStakingInfo : UserStakingInfo
    , time : Time.Posix
    }


type alias DepositWithdrawUXModel =
    { inMenu : Maybe DepositOrWithdraw
    }


type DepositOrWithdraw
    = Deposit
    | Withdraw


calcAvailableRewards : TimedUserStakingInfo -> Time.Posix -> TokenValue
calcAvailableRewards stakingInfo now =
    TokenValue.zero
