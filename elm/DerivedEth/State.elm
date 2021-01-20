module DerivedEth.State exposing (..)

import Common.Msg exposing (MsgDown, gTag)
import Common.Types exposing (..)
import Config exposing (derivedEthContractAddress, forbiddenJurisdictionCodes)
import Contracts.DEthWrapper as Death
import Contracts.ERC20Wrapper as ERC20
import DerivedEth.Types exposing (..)
import Eth.Types exposing (Address)
import Eth.Utils
import Json.Decode
import Ports exposing (beginLocationCheck, locationCheckResult)
import Result.Extra
import Set
import Time
import TokenValue exposing (TokenValue)
import UserTx
import Wallet exposing (Wallet, userInfo)


init :
    Wallet
    -> Time.Posix
    -> ( Model, Cmd Msg )
init wallet now =
    ( { now = now
      , wallet = wallet
      , userDerivedEthInfo = Nothing
      , jurisdictionCheckStatus = WaitingForClick
      , depositAmount = ""
      , withDrawalAmount = ""
      }
    , [ wallet
            |> fetchDerivedEthBalance
      , wallet
            |> fetchEthBalance
      ]
        |> Cmd.batch
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
                []
                [ msgUp ]

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

        DepositClicked amount ->
            let
                userInfo =
                    prevModel.wallet
                        |> Wallet.userInfo
            in
            UpdateResult
                prevModel
                Cmd.none
                (case userInfo of
                    Nothing ->
                        []

                    Just uInfo ->
                        [ doDepositChainCmd
                            uInfo.address
                            amount
                        ]
                )
                [ Common.Msg.GTag <|
                    GTagData
                        "Squander ETH"
                        "funnel"
                        ""
                        (TokenValue.mul 100 amount
                            |> TokenValue.toFloatWithWarning
                            |> floor
                        )
                ]

        WithdrawClicked amount ->
            let
                userInfo =
                    prevModel.wallet
                        |> Wallet.userInfo
            in
            UpdateResult
                prevModel
                Cmd.none
                (case userInfo of
                    Nothing ->
                        []

                    Just uInfo ->
                        [ doWithdrawChainCmd
                            uInfo.address
                            amount
                        ]
                )
                []

        UserEthBalanceFetched fetchResult ->
            case fetchResult of
                Err _ ->
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
                                        , dEthAllowance = TokenValue.zero
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
                Err _ ->
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
                                        , dEthAllowance = TokenValue.zero
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
                Err _ ->
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
                                        , dEthAllowance = TokenValue.zero
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

        ApproveTokenSpend ->
            UpdateResult
                prevModel
                Cmd.none
                []
                []

        DepositSigned signResult ->
            case signResult of
                Ok txHash ->
                    let
                        tv =
                            TokenValue.fromString prevModel.depositAmount

                        amount =
                            case tv of
                                Nothing ->
                                    TokenValue.zero

                                Just val ->
                                    val
                    in
                    UpdateResult
                        { prevModel
                            | depositAmount = ""
                        }
                        Cmd.none
                        []
                        [ Common.Msg.GTag <|
                            GTagData
                                "Squander ETH Signed"
                                "conversion"
                                (Eth.Utils.txHashToString txHash)
                                (amount
                                    |> TokenValue.mul 100
                                    |> TokenValue.toFloatWithWarning
                                    |> floor
                                )
                        ]

                _ ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        []
                        []

        WithdrawSigned signResult ->
            case signResult of
                Ok txHash ->
                    let
                        tv =
                            TokenValue.fromString prevModel.depositAmount

                        amount =
                            case tv of
                                Nothing ->
                                    TokenValue.zero

                                Just val ->
                                    val
                    in
                    UpdateResult
                        { prevModel
                            | withDrawalAmount = ""
                        }
                        Cmd.none
                        []
                        [ Common.Msg.GTag <|
                            GTagData
                                "Redeem worthless beans Signed"
                                "conversion"
                                (Eth.Utils.txHashToString txHash)
                                (amount
                                    |> TokenValue.mul 100
                                    |> TokenValue.toFloatWithWarning
                                    |> floor
                                )
                        ]

                _ ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        []
                        []

        FetchUserEthBalance ->
            UpdateResult
                prevModel
                (prevModel.wallet
                    |> fetchEthBalance
                )
                []
                []

        FetchUserDerivedEthBalance ->
            UpdateResult
                prevModel
                (prevModel.wallet
                    |> fetchDerivedEthBalance
                )
                []
                []

        Tick _ ->
            let
                userInfo =
                    prevModel.wallet
                        |> Wallet.userInfo
            in
            UpdateResult
                prevModel
                ((case userInfo of
                    Nothing ->
                        []

                    Just _ ->
                        [ prevModel.wallet
                            |> fetchDerivedEthBalance
                        , prevModel.wallet
                            |> fetchEthBalance
                        , prevModel.userDerivedEthInfo
                            |> fetchDethPositionInfo
                        ]
                 )
                    |> Cmd.batch
                )
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
                        Just _ ->
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


fetchEthBalance :
    Wallet
    -> Cmd Msg
fetchEthBalance wallet =
    case Wallet.userInfo wallet of
        Nothing ->
            Cmd.none

        Just userInfo ->
            ERC20.getEthBalance
                userInfo.address
                UserEthBalanceFetched


fetchDerivedEthBalance :
    Wallet
    -> Cmd Msg
fetchDerivedEthBalance wallet =
    case Wallet.userInfo wallet of
        Nothing ->
            Cmd.none

        Just userInfo ->
            ERC20.getBalanceCmd
                derivedEthContractAddress
                userInfo.address
                UserDerivedEthBalanceFetched


fetchDethPositionInfo :
    Maybe UserDerivedEthInfo
    -> Cmd Msg
fetchDethPositionInfo maybeDethInfo =
    case maybeDethInfo of
        Nothing ->
            Cmd.none

        Just dEthInfo ->
            Death.getRedeemable
                dEthInfo.dEthBalance
                DerivedEthRedeemableFetched


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


locationCheckResultToJurisdictionStatus :
    Result Json.Decode.Error (Result String LocationInfo)
    -> JurisdictionCheckStatus
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


countryCodeToJurisdiction :
    String
    -> String
    -> Jurisdiction
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


doDepositChainCmd :
    Address
    -> TokenValue
    -> UserTx.Initiator Msg
doDepositChainCmd sender amount =
    { notifiers =
        { onMine =
            always FetchUserEthBalance
                |> Just
        , onSign =
            DepositSigned
                |> Just
        }
    , send = Death.deposit sender amount
    , txInfo = UserTx.DEthDeposit
    }


doWithdrawChainCmd :
    Address
    -> TokenValue
    -> UserTx.Initiator Msg
doWithdrawChainCmd receiver amount =
    { notifiers =
        { onMine =
            always FetchUserDerivedEthBalance
                |> Just
        , onSign =
            WithdrawSigned
                |> Just
        }
    , send = Death.redeem receiver amount
    , txInfo = UserTx.DEthRedeem
    }


subscriptions :
    Model
    -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every (1000 * 3) Tick
        , locationCheckResult
            (Json.Decode.decodeValue locationCheckDecoder >> LocationCheckResult)
        ]
