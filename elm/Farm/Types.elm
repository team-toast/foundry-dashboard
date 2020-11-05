module Farm.Types exposing (..)

import Eth.Types exposing (Address)
import Common.Msg exposing (..)
import Common.Types exposing (..)
import Http
import Time
import TokenValue exposing (TokenValue)


type alias Model =
    { timedUserStakingInfo : Maybe TimedUserStakingInfo
    , depositWithdrawUXModel : DepositOrWithdrawUXModel
    , now : Time.Posix
    }


type Msg
    = MsgUp MsgUp
    | NoOp
    | UpdateNow Time.Posix
    | StartDeposit
    | DoDeposit TokenValue
    | DoExit
    | StartWithdraw
    | DoWithdraw TokenValue
    | StakingInfoFetched (Result Http.Error UserStakingInfo)
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


type alias DepositOrWithdrawUXModel =
    Maybe (DepositOrWithdraw, AmountUXModel)

type alias AmountUXModel =
    { amountInput : String
    }


type DepositOrWithdraw
    = Deposit
    | Withdraw


calcAvailableRewards : TimedUserStakingInfo -> Time.Posix -> TokenValue
calcAvailableRewards stakingInfo now =
    TokenValue.zero
