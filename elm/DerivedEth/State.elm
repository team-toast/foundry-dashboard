module DerivedEth.State exposing (..)

import Array
import Common.Msg exposing (..)
import Common.Types exposing (..)
import Config
import Contracts.ERC20Wrapper as ERC20
import DerivedEth.Types exposing (..)
import Eth.Types exposing (Address)
import Helpers.Eth
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
                { prevModel
                    | depositAmount = amount
                }
                Cmd.none
                []
                []

        WithdrawalAmountChanged amount ->
            UpdateResult
                { prevModel
                    | withDrawalAmount = amount
                }
                Cmd.none
                []
                []

        DepositClicked ->
            UpdateResult
                prevModel
                Cmd.none
                []
                []

        WithdrawClicked ->
            UpdateResult
                prevModel
                Cmd.none
                []
                []

        UserEthBalanceFetched fetchResult ->
            case fetchResult of
                Err err ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        []
                        []

                Ok tokenValue ->
                    UpdateResult
                        { prevModel
                            | userDerivedEthInfo =
                                (case prevModel.userDerivedEthInfo of
                                    Nothing ->
                                        { ethBalance = tokenValue
                                        , dEthBalance = TokenValue.zero
                                        , totalCollateralRedeemed = TokenValue.zero
                                        , fee = TokenValue.zero
                                        , collateralReturned = TokenValue.zero
                                        }

                                    Just oldUserDerivedEthInfoModel ->
                                        { oldUserDerivedEthInfoModel
                                            | ethBalance = tokenValue
                                        }
                                )
                                    |> Just
                        }
                        Cmd.none
                        []
                        []

        UserDerivedEthBalanceFetched fetchResult ->
            case fetchResult of
                Err err ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        []
                        []

                Ok tokenValue ->
                    UpdateResult
                        { prevModel
                            | userDerivedEthInfo =
                                (case prevModel.userDerivedEthInfo of
                                    Nothing ->
                                        { ethBalance = TokenValue.zero
                                        , dEthBalance = tokenValue
                                        , totalCollateralRedeemed = TokenValue.zero
                                        , fee = TokenValue.zero
                                        , collateralReturned = TokenValue.zero
                                        }

                                    Just oldUserDerivedEthInfoModel ->
                                        { oldUserDerivedEthInfoModel
                                            | dEthBalance = tokenValue
                                        }
                                )
                                    |> Just
                        }
                        Cmd.none
                        []
                        []

        DerivedEthRedeemableFetched fetchResult ->
            case fetchResult of
                Err err ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        []
                        []

                Ok ( totalCollateral, fee, returnedCollateral ) ->
                    UpdateResult
                        { prevModel
                            | userDerivedEthInfo =
                                (case prevModel.userDerivedEthInfo of
                                    Nothing ->
                                        { ethBalance = TokenValue.zero
                                        , dEthBalance = TokenValue.zero
                                        , totalCollateralRedeemed = totalCollateral
                                        , fee = fee
                                        , collateralReturned = returnedCollateral
                                        }

                                    Just oldUserDerivedEthInfoModel ->
                                        { oldUserDerivedEthInfoModel
                                            | totalCollateralRedeemed = totalCollateral
                                            , fee = fee
                                            , collateralReturned = returnedCollateral
                                        }
                                )
                                    |> Just
                        }
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


fetchEthBalance :
    Address
    -> Cmd Msg
fetchEthBalance owner =
    ERC20.getBalanceCmd
        Helpers.Eth.zeroAddress
        owner
        UserEthBalanceFetched


fetchDerivedEthBalance :
    Address
    -> Cmd Msg
fetchDerivedEthBalance owner =
    ERC20.getBalanceCmd
        Config.derivedEthContractAddress
        owner
        UserDerivedEthBalanceFetched
