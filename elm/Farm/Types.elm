module Farm.Types exposing (..)

import UserTx exposing (TxInfo)
import Common.Msg exposing (..)
import Common.Types exposing (..)
import Eth.Types exposing (Address, TxHash)
import Helpers.Time as TimeHelpers
import Http
import Time
import TokenValue exposing (TokenValue)
import Wallet exposing (Wallet)


type alias Model =
    { wallet : Wallet.Wallet
    , userStakingInfo : Maybe UserStakingInfo
    , apy : Maybe Float
    , depositWithdrawUXModel : DepositOrWithdrawUXModel
    , now : Time.Posix
    }


type Msg
    = MsgUp MsgUp
    | NoOp
    | UpdateNow Time.Posix
    | AmountInputChanged String
    | UXBack
    | StartDeposit TokenValue
    | DoUnlock
    | DoDeposit TokenValue
    | DoClaimRewards
    | DoExit
    | StartWithdraw TokenValue
    | DoWithdraw TokenValue
    | DepositOrWithdrawSigned DepositOrWithdraw TokenValue (Result String TxHash)
    | StakingInfoFetched (Result Http.Error (UserStakingInfo, Float))
    | ApyFetched (Result Http.Error Float)
    | RefetchStakingInfoOrApy


type alias UpdateResult =
    { newModel : Model
    , cmd : Cmd Msg
    , userTxs : List (UserTx.Initiator Msg)
    , msgUps : List MsgUp
    }


justModelUpdate : Model -> UpdateResult
justModelUpdate model =
    { newModel = model
    , cmd = Cmd.none
    , userTxs = []
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


validateInput : String -> TokenValue -> Maybe TokenValue
validateInput input max =
    TokenValue.fromString input
        |> Maybe.andThen
            (\val ->
                if TokenValue.compare val TokenValue.zero == LT then
                    Nothing

                else if TokenValue.compare val max == GT then
                    Nothing

                else
                    Just val
            )
