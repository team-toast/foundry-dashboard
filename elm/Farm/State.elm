port module Farm.State exposing (..)

import Common.Msg exposing (MsgDown, MsgUp, gTag)
import Common.Types exposing (..)
import Config exposing (forbiddenJurisdictionCodes)
import Contracts.Staking as StakingContract
import Eth.Types exposing (Address)
import Eth.Utils
import Farm.Types exposing (..)
import Html exposing (th)
import Json.Decode
import Maybe
import Result.Extra
import Set
import Time
import TokenValue exposing (TokenValue)
import UserNotice as UN
import UserTx exposing (TxInfo)
import Wallet exposing (Wallet)


init : Wallet -> Time.Posix -> ( Model, Cmd Msg )
init wallet now =
    ( { wallet = wallet
      , userStakingInfo = Nothing
      , depositWithdrawUXModel = Nothing
      , apy = Nothing
      , now = now
      , jurisdictionCheckStatus = WaitingForClick
      }
    , fetchStakingInfoOrApyCmd now wallet
    )


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        NoOp ->
            justModelUpdate prevModel

        MsgUp msgUp ->
            UpdateResult
                prevModel
                Cmd.none
                []
                [ msgUp ]

        UpdateNow newNow ->
            if calcTimeLeft newNow <= 0 then
                justModelUpdate prevModel

            else
                justModelUpdate
                    { prevModel | now = newNow }

        AmountInputChanged newInput ->
            case prevModel.depositWithdrawUXModel of
                Just ( depositOrWithdraw, amountInputUXModel ) ->
                    justModelUpdate
                        { prevModel
                            | depositWithdrawUXModel =
                                Just
                                    ( depositOrWithdraw
                                    , { amountInputUXModel
                                        | amountInput = newInput
                                      }
                                    )
                        }

                Nothing ->
                    justModelUpdate prevModel

        UXBack ->
            justModelUpdate
                { prevModel
                    | depositWithdrawUXModel = Nothing
                }

        DoUnlock ->
            UpdateResult
                prevModel
                Cmd.none
                [ doApproveChainCmd ]
                []

        StartDeposit defaultValue ->
            justModelUpdate
                { prevModel
                    | depositWithdrawUXModel = Just ( Deposit, { amountInput = TokenValue.toFloatString Nothing defaultValue } )
                }

        StartWithdraw defaultValue ->
            justModelUpdate
                { prevModel
                    | depositWithdrawUXModel = Just ( Withdraw, { amountInput = TokenValue.toFloatString Nothing defaultValue } )
                }

        DoExit ->
            UpdateResult
                prevModel
                Cmd.none
                [ doExitChainCmd ]
                []

        DoClaimRewards ->
            UpdateResult
                prevModel
                Cmd.none
                [ doClaimRewards ]
                []

        DoDeposit amount ->
            UpdateResult
                prevModel
                Cmd.none
                [ doDepositChainCmd amount ]
                [ Common.Msg.GTag <|
                    GTagData
                        "Deposit Liquidity"
                        "funnel"
                        ""
                        (TokenValue.mul 100 amount
                            |> TokenValue.toFloatWithWarning
                            |> floor
                        )
                ]

        DoWithdraw amount ->
            UpdateResult
                prevModel
                Cmd.none
                [ doWithdrawChainCmd amount ]
                []

        DepositOrWithdrawSigned depositOrWithdraw amount signResult ->
            case signResult of
                Ok txHash ->
                    UpdateResult
                        { prevModel
                            | depositWithdrawUXModel = Nothing
                        }
                        Cmd.none
                        []
                        (case depositOrWithdraw of
                            Deposit ->
                                [ Common.Msg.GTag <|
                                    GTagData
                                        "Deposit Liquidity Signed"
                                        "conversion"
                                        (Eth.Utils.txHashToString txHash)
                                        (TokenValue.mul 100 amount
                                            |> TokenValue.toFloatWithWarning
                                            |> floor
                                        )
                                ]

                            Withdraw ->
                                []
                        )

                _ ->
                    justModelUpdate prevModel

        RefetchStakingInfoOrApy ->
            if calcTimeLeft prevModel.now <= 0 then
                justModelUpdate prevModel

            else
                UpdateResult
                    prevModel
                    (fetchStakingInfoOrApyCmd prevModel.now prevModel.wallet)
                    []
                    []

        StakingInfoFetched fetchResult ->
            case fetchResult of
                Err httpErr ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        []
                        [ Common.Msg.AddUserNotice <| UN.web3FetchError "staking info" httpErr ]

                Ok ( userStakingInfo, apy ) ->
                    justModelUpdate
                        { prevModel
                            | userStakingInfo = Just userStakingInfo
                            , apy = Just apy
                        }

        ApyFetched fetchResult ->
            if calcTimeLeft prevModel.now <= 0 then
                justModelUpdate prevModel

            else
                case fetchResult of
                    Err httpErr ->
                        UpdateResult
                            prevModel
                            Cmd.none
                            []
                            [ Common.Msg.AddUserNotice <| UN.web3FetchError "apy" httpErr ]

                    Ok apy ->
                        justModelUpdate
                            { prevModel
                                | apy =
                                    Just apy
                            }

        VerifyJurisdictionClicked ->
            UpdateResult
                { prevModel
                    | jurisdictionCheckStatus = Checking
                }
                (beginLocationCheck ())
                []
                [ gTag
                    "3a - verify jurisdiction clicked"
                    "funnel"
                    ""
                    0
                ]

        LocationCheckResult decodeResult ->
            let
                jurisdictionCheckStatus =
                    locationCheckResultToJurisdictionStatus decodeResult
            in
            UpdateResult
                { prevModel
                    | jurisdictionCheckStatus = jurisdictionCheckStatus
                }
                Cmd.none
                []
                (case jurisdictionCheckStatus of
                    WaitingForClick ->
                        []

                    Checking ->
                        []

                    Checked ForbiddenJurisdictions ->
                        [ gTag
                            "jurisdiction not allowed"
                            "funnel abort"
                            ""
                            0
                        ]

                    Checked _ ->
                        [ gTag
                            "3b - jurisdiction verified"
                            "funnel"
                            ""
                            0
                        ]

                    Error error ->
                        [ gTag
                            "failed jursidiction check"
                            "funnel abort"
                            error
                            0
                        ]
                )


locationCheckResultToJurisdictionStatus : Result Json.Decode.Error (Result String LocationInfo) -> JurisdictionCheckStatus
locationCheckResultToJurisdictionStatus decodeResult =
    decodeResult
        |> Result.map
            (\checkResult ->
                checkResult
                    |> Result.map
                        (\locationInfo ->
                            Checked <|
                                countryCodeToJurisdiction locationInfo.ipCode locationInfo.geoCode
                        )
                    |> Result.mapError
                        (\e ->
                            Error <|
                                "Location check failed: "
                                    ++ e
                        )
                    |> Result.Extra.merge
            )
        |> Result.mapError
            (\e -> Error <| "Location check response decode error: " ++ Json.Decode.errorToString e)
        |> Result.Extra.merge


countryCodeToJurisdiction : String -> String -> Jurisdiction
countryCodeToJurisdiction ipCode geoCode =
    let
        allowedJurisdiction =
            Set.fromList [ ipCode, geoCode ]
                |> Set.intersect forbiddenJurisdictionCodes
                |> Set.isEmpty
    in
    if allowedJurisdiction then
        JurisdictionsWeArentIntimidatedIntoExcluding

    else
        ForbiddenJurisdictions


locationCheckDecoder : Json.Decode.Decoder (Result String LocationInfo)
locationCheckDecoder =
    Json.Decode.oneOf
        [ Json.Decode.field "errorMessage" Json.Decode.string
            |> Json.Decode.map Err
        , locationInfoDecoder
            |> Json.Decode.map Ok
        ]


locationInfoDecoder : Json.Decode.Decoder LocationInfo
locationInfoDecoder =
    Json.Decode.map2
        LocationInfo
        (Json.Decode.field "ipCountry" Json.Decode.string)
        (Json.Decode.field "geoCountry" Json.Decode.string)


runMsgDown : MsgDown -> Model -> UpdateResult
runMsgDown msg prevModel =
    case msg of
        Common.Msg.UpdateWallet newWallet ->
            let
                newModel =
                    { prevModel
                        | wallet = newWallet
                        , userStakingInfo = Nothing
                    }

                cmd =
                    case Wallet.userInfo newWallet of
                        Just userInfo ->
                            fetchUserStakingInfoCmd userInfo.address

                        Nothing ->
                            Cmd.none
            in
            UpdateResult
                newModel
                cmd
                []
                []


fetchStakingInfoOrApyCmd : Time.Posix -> Wallet -> Cmd Msg
fetchStakingInfoOrApyCmd now wallet =
    case Wallet.userInfo wallet of
        Just userInfo ->
            fetchUserStakingInfoCmd userInfo.address

        Nothing ->
            fetchApyCmd


fetchUserStakingInfoCmd : Address -> Cmd Msg
fetchUserStakingInfoCmd userAddress =
    StakingContract.getUserStakingInfo
        userAddress
        StakingInfoFetched


fetchApyCmd : Cmd Msg
fetchApyCmd =
    StakingContract.getApy
        ApyFetched


doApproveChainCmd : UserTx.Initiator Msg
doApproveChainCmd =
    { notifiers =
        { onMine = Just <| always RefetchStakingInfoOrApy
        , onSign = Nothing
        }
    , send = StakingContract.approveLiquidityToken
    , txInfo = UserTx.StakingApprove
    }


doDepositChainCmd : TokenValue -> UserTx.Initiator Msg
doDepositChainCmd amount =
    { notifiers =
        { onMine = Just <| always RefetchStakingInfoOrApy
        , onSign = Just <| DepositOrWithdrawSigned Deposit amount
        }
    , send = StakingContract.stake amount
    , txInfo = UserTx.StakingDeposit amount
    }


doWithdrawChainCmd : TokenValue -> UserTx.Initiator Msg
doWithdrawChainCmd amount =
    { notifiers =
        { onMine = Just <| always RefetchStakingInfoOrApy
        , onSign = Just <| DepositOrWithdrawSigned Withdraw amount
        }
    , send = StakingContract.withdraw amount
    , txInfo = UserTx.StakingWithdraw amount
    }


doExitChainCmd : UserTx.Initiator Msg
doExitChainCmd =
    { notifiers =
        { onMine = Just <| always RefetchStakingInfoOrApy
        , onSign = Nothing
        }
    , send = StakingContract.exit
    , txInfo = UserTx.StakingExit
    }


doClaimRewards : UserTx.Initiator Msg
doClaimRewards =
    { notifiers =
        { onMine = Just <| always RefetchStakingInfoOrApy
        , onSign = Nothing
        }
    , send = StakingContract.claimRewards
    , txInfo = UserTx.StakingClaim
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 50 UpdateNow
        , locationCheckResult
            (Json.Decode.decodeValue locationCheckDecoder >> LocationCheckResult)
        , Time.every 10000 (always RefetchStakingInfoOrApy)
        ]


port beginLocationCheck : () -> Cmd msg


port locationCheckResult : (Json.Decode.Value -> msg) -> Sub msg
