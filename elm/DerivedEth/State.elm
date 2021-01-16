module DerivedEth.State exposing (..)

import Array
import Common.Msg exposing (..)
import Common.Types exposing (..)
import Config
import DerivedEth.Types exposing (..)
import Eth.Types exposing (Address)
import Http
import Json.Decode exposing (Decoder, value)
import Time
import TokenValue
import Wallet exposing (Wallet)


init :
    Wallet
    -> Time.Posix
    -> ( Model, Cmd Msg )
init wallet now =
    ( { now = now
      , wallet = wallet
      , userDerivedEthInfo = Nothing
      , jurisdictionCheckStatus = WaitingForClick
      , depositAmount = "0"
      , withDrawalAmount = "0"
      }
    , Cmd.none
    )


update :
    Msg
    -> Model
    -> UpdateResult
update msg prevModel =
    case msg of
        MsgUp msgUp ->
            UpdateResult
                prevModel
                Cmd.none
                [ msgUp ]
                []

        DepositAmountChanged amount ->
            UpdateResult
                { prevModel | depositAmount = amount }
                Cmd.none
                []
                []

        WithdrawalAmountChanged amount ->
            UpdateResult
                { prevModel | withDrawalAmount = amount }
                Cmd.none
                []
                []

        Tick i ->
            UpdateResult
                prevModel
                Cmd.none
                []
                []


runMsgDown :
    MsgDown
    -> Model
    -> UpdateResult
runMsgDown msg prevModel =
    case msg of
        Common.Msg.UpdateWallet newWallet ->
            let
                newModel =
                    { prevModel
                        | wallet = newWallet
                        , userDerivedEthInfo = Nothing
                    }

                cmd =
                    case Wallet.userInfo newWallet of
                        Just userInfo ->
                            Cmd.none

                        --fetchUserDerivedEthInfoCmd userInfo.address
                        Nothing ->
                            Cmd.none
            in
            UpdateResult
                newModel
                cmd
                []
                []


subscriptions :
    Model
    -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (1000 * 60 * 5) Tick -- (1000 * 60 * 5) -> 5 minutes
        ]
