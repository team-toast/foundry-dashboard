module DerivedEth.State exposing (..)

import Array
import Common.Msg exposing (MsgDown, MsgUp, gTag)
import Common.Types exposing (..)
import Config exposing (forbiddenJurisdictionCodes)
import Contracts.ERC20Wrapper as ERC20
import DerivedEth.Types exposing (..)
import Eth.Types exposing (Address)
import Helpers.Eth
import Http
import Json.Decode exposing (Decoder, value)
import Ports exposing (beginLocationCheck, locationCheckResult)
import Result.Extra
import Set exposing (Set)
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
      , depositAmount = ""
      , withDrawalAmount = ""
      }
    , [ fetchEthBalance wallet
      , fetchDerivedEthBalance wallet
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

        Tick i ->
            UpdateResult
                prevModel
                ([ fetchDerivedEthBalance prevModel.wallet
                 , fetchEthBalance prevModel.wallet
                 ]
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


fetchEthBalance :
    Wallet
    -> Cmd Msg
fetchEthBalance wallet =
    case Wallet.userInfo wallet of
        Just userInfo ->
            ERC20.getBalanceCmd
                Helpers.Eth.zeroAddress
                userInfo.address
                UserEthBalanceFetched

        Nothing ->
            Cmd.none


fetchDerivedEthBalance :
    Wallet
    -> Cmd Msg
fetchDerivedEthBalance wallet =
    case Wallet.userInfo wallet of
        Just userInfo ->
            ERC20.getBalanceCmd
                Config.derivedEthContractAddress
                userInfo.address
                UserDerivedEthBalanceFetched

        Nothing ->
            Cmd.none


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


subscriptions :
    Model
    -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (1000 * 60 * 5 / 10) Tick -- (1000 * 60 * 5) -> 5 minutes
        , locationCheckResult
            (Json.Decode.decodeValue locationCheckDecoder >> LocationCheckResult)
        ]
