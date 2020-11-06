module Farm.Types exposing (..)

import ChainCmd exposing (ChainCmd)
import Common.Msg exposing (..)
import Common.Types exposing (..)
import Eth.Types exposing (Address)
import Helpers.Time as TimeHelpers
import Http
import Time
import TokenValue exposing (TokenValue)


type alias Model =
    { userStakingInfo : Maybe UserStakingInfo
    , depositWithdrawUXModel : DepositOrWithdrawUXModel
    , now : Time.Posix
    }


type Msg
    = MsgUp MsgUp
    | NoOp
    | UpdateNow Time.Posix
    | AmountInputChanged String
    | StartDeposit
    | DoUnlock
    | DoDeposit TokenValue
    | DoClaimRewards
    | DoExit
    | StartWithdraw
    | DoWithdraw TokenValue
    | StakingInfoFetched (Result Http.Error UserStakingInfo)
    | FakeFetchBalanceInfo


type alias UpdateResult =
    { newModel : Model
    , cmd : Cmd Msg
    , chainCmd : ChainCmd Msg
    , msgUps : List MsgUp
    }


justModelUpdate : Model -> UpdateResult
justModelUpdate model =
    { newModel = model
    , cmd = Cmd.none
    , chainCmd = ChainCmd.none
    , msgUps = []
    }


type alias DepositOrWithdrawUXModel =
    Maybe ( DepositOrWithdraw, AmountUXModel )


type alias AmountUXModel =
    { amountInput : String
    }


type DepositOrWithdraw
    = Deposit
    | Withdraw


calcAvailableRewards : UserStakingInfo -> Time.Posix -> TokenValue
calcAvailableRewards stakingInfo now =
    let
        secondsElapsed =
            TimeHelpers.sub now stakingInfo.timestamp
                |> Time.posixToMillis
                |> toFloat
                |> (\msec -> msec / 1000)

        accrued =
            TokenValue.mulFloatWithWarning stakingInfo.rewardRate secondsElapsed
    in
    TokenValue.add stakingInfo.claimableRewards accrued


validateInput : String -> Maybe TokenValue
validateInput input =
    TokenValue.fromString input
        |> Maybe.andThen
            (\val ->
                if TokenValue.compare val TokenValue.zero == LT then
                    Nothing

                else
                    Just val
            )
