module Update exposing (..)

import AddressDict exposing (AddressDict)
import Array exposing (Array)
import Browser
import Browser.Navigation
import Config
import Dict
import Dict.Extra
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Sentry.Tx as TxSentry exposing (TxSentry)
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..))
import Json.Decode
import List.Extra
import Maybe.Extra
import Misc exposing (..)
import Ports
import Routing
import Time
import TokenValue
import Types exposing (..)
import Url
import UserNotice exposing (cantConnectNoWeb3, signingError, unexpectedError, walletError, web3FetchError)
import UserTx


update : Msg -> Model -> ( Model, Cmd Msg )
update msg prevModel =
    case msg of
        LinkClicked urlRequest ->
            let
                cmd =
                    case urlRequest of
                        Browser.Internal url ->
                            Browser.Navigation.pushUrl prevModel.navKey (Url.toString url)

                        Browser.External href ->
                            Browser.Navigation.load href
            in
            ( prevModel, cmd )

        UrlChanged url ->
            prevModel |> updateFromPageRoute (url |> Routing.urlToRoute)

        Tick newTime ->
            ( { prevModel | now = newTime }, Cmd.none )

        Resize width _ ->
            ( { prevModel
                | dProfile =
                    EH.screenWidthToDisplayProfile width
              }
            , Cmd.none
            )

        EveryFewSeconds ->
            ( prevModel
            , Cmd.none
            )

        WalletStatus walletSentryResult ->
            case walletSentryResult of
                Ok walletSentry ->
                    let
                        ( newWallet, notifySubmodel ) =
                            case walletSentry.account of
                                Just newAddress ->
                                    if
                                        (prevModel.wallet
                                            |> userInfo
                                            |> Maybe.map .address
                                        )
                                            == Just newAddress
                                    then
                                        ( prevModel.wallet, False )

                                    else
                                        ( UserInfo
                                            walletSentry.networkId
                                            newAddress
                                            |> Types.Active
                                        , True
                                        )

                                Nothing ->
                                    ( OnlyNetwork walletSentry.networkId
                                    , prevModel.wallet /= OnlyNetwork walletSentry.networkId
                                    )
                    in
                    { prevModel
                        | wallet = newWallet
                    }
                        |> (if notifySubmodel then
                                runMsgDown (UpdateWallet newWallet)

                            else
                                \model -> ( model, Cmd.none )
                           )

                Err errStr ->
                    ( prevModel
                        |> addUserNotice (walletError errStr)
                    , Cmd.none
                    )

        TxSentryMsg subMsg ->
            let
                ( newTxSentry, subCmd ) =
                    TxSentry.update subMsg prevModel.txSentry
            in
            ( { prevModel | txSentry = newTxSentry }, subCmd )

        EventSentryMsg eventMsg ->
            let
                ( newEventSentry, cmd ) =
                    EventSentry.update
                        eventMsg
                        prevModel.eventSentry
            in
            ( { prevModel
                | eventSentry =
                    newEventSentry
              }
            , cmd
            )

        DismissNotice id ->
            ( { prevModel
                | userNotices =
                    prevModel.userNotices |> List.Extra.removeAt id
              }
            , Cmd.none
            )

        ShowExpandedTrackedTxs flag ->
            ( { prevModel
                | trackedTxsExpanded = flag
              }
            , Cmd.none
            )

        TxSigned trackedTxId signResult ->
            let
                newTrackedTxs =
                    prevModel.trackedTxs
                        |> UserTx.setTrackedTxStatus trackedTxId
                            (case signResult of
                                Err errStr ->
                                    UserTx.Rejected

                                Ok txHash ->
                                    UserTx.Signed txHash UserTx.Mining
                            )

                maybeExtraMsgConstructor =
                    prevModel.trackedTxs
                        |> List.Extra.getAt trackedTxId
                        |> Maybe.map .notifiers
                        |> Maybe.andThen .onSign

                intermediateModel =
                    { prevModel
                        | trackedTxs = newTrackedTxs
                    }
            in
            case maybeExtraMsgConstructor of
                Just extraMsgConstructor ->
                    intermediateModel
                        |> update (extraMsgConstructor signResult)

                Nothing ->
                    ( intermediateModel
                    , Cmd.none
                    )

        TxMined trackedTxId mineResult ->
            let
                newTrackedTxs =
                    prevModel.trackedTxs
                        |> UserTx.setTrackedTxSignedStatus trackedTxId
                            (case mineResult of
                                Err errStr ->
                                    UserTx.Failed

                                Ok txReceipt ->
                                    UserTx.Success txReceipt
                            )

                maybeExtraMsgConstructor =
                    prevModel.trackedTxs
                        |> List.Extra.getAt trackedTxId
                        |> Maybe.map .notifiers
                        |> Maybe.andThen .onMine

                intermediateModel =
                    { prevModel
                        | trackedTxs = newTrackedTxs
                    }
            in
            case maybeExtraMsgConstructor of
                Just extraMsgConstructor ->
                    intermediateModel
                        |> update (extraMsgConstructor mineResult)

                Nothing ->
                    ( intermediateModel
                    , Cmd.none
                    )

        CookieConsentGranted ->
            ( { prevModel
                | cookieConsentGranted = True
              }
            , Cmd.batch
                [ Ports.consentToCookies ()
                , Ports.gTagOut <|
                    encodeGTag <|
                        GTagData
                            "accept cookies"
                            ""
                            ""
                            0
                ]
            )

        ClickHappened ->
            ( { prevModel
                | showAddressId = Nothing
              }
            , Cmd.none
            )

        Types.NoOp ->
            ( prevModel, Cmd.none )

        FetchedEthPrice fetchResult ->
            case fetchResult of
                Err error ->
                    UpdateResult
                        prevModel
                        Cmd.none

                Ok bundle1 ->
                    let
                        v =
                            case bundle1 of
                                Just val ->
                                    val

                                Nothing ->
                                    Value 0
                    in
                    UpdateResult
                        { prevModel
                            | currentEthPriceUsd =
                                Just
                                    v.ethPrice
                            , circSupply =
                                calcCircSupply
                                    prevModel.currentBucketId
                                    prevModel.teamTokenBalances
                                    prevModel.permaFrostedTokens
                            , marketCap =
                                calcMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                                    prevModel.circSupply
                            , fullyDiluted =
                                calcFullyDilutedMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                        }
                        Cmd.none

        FetchedDaiPrice fetchResult ->
            case fetchResult of
                Err error ->
                    UpdateResult
                        prevModel
                        Cmd.none

                Ok value ->
                    let
                        v =
                            case value of
                                Just val ->
                                    val

                                Nothing ->
                                    Value 0
                    in
                    UpdateResult
                        { prevModel
                            | currentDaiPriceEth =
                                Just
                                    v.ethPrice
                            , circSupply =
                                calcCircSupply
                                    prevModel.currentBucketId
                                    prevModel.teamTokenBalances
                                    prevModel.permaFrostedTokens
                            , marketCap =
                                calcMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                                    prevModel.circSupply
                            , fullyDiluted =
                                calcFullyDilutedMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                        }
                        Cmd.none

        FetchedFryPrice fetchResult ->
            case fetchResult of
                Err error ->
                    UpdateResult
                        prevModel
                        Cmd.none

                Ok value ->
                    let
                        v =
                            case value of
                                Just val ->
                                    val

                                Nothing ->
                                    Value 0
                    in
                    UpdateResult
                        { prevModel
                            | currentFryPriceEth =
                                Just
                                    v.ethPrice
                            , circSupply =
                                calcCircSupply
                                    prevModel.currentBucketId
                                    prevModel.teamTokenBalances
                                    prevModel.permaFrostedTokens
                            , marketCap =
                                calcMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                                    prevModel.circSupply
                            , fullyDiluted =
                                calcFullyDilutedMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                        }
                        Cmd.none

        FetchedTeamTokens index fetchResult ->
            case fetchResult of
                Err error ->
                    UpdateResult
                        prevModel
                        Cmd.none

                Ok value ->
                    UpdateResult
                        { prevModel
                            | teamTokenBalances = Array.set index (Just value) prevModel.teamTokenBalances
                            , circSupply =
                                calcCircSupply
                                    prevModel.currentBucketId
                                    prevModel.teamTokenBalances
                                    prevModel.permaFrostedTokens
                            , marketCap =
                                calcMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                                    prevModel.circSupply
                            , fullyDiluted =
                                calcFullyDilutedMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                        }
                        Cmd.none

        FetchedBalancerFryBalance fetchResult ->
            case fetchResult of
                Err error ->
                    UpdateResult
                        prevModel
                        Cmd.none

                Ok value ->
                    UpdateResult
                        { prevModel
                            | balancerFryBalance = Just value
                            , permaFrostedTokens =
                                calcPermaFrostedTokens
                                    (Just value)
                                    prevModel.permaFrostBalanceLocked
                                    prevModel.permaFrostTotalSupply
                            , circSupply =
                                calcCircSupply
                                    prevModel.currentBucketId
                                    prevModel.teamTokenBalances
                                    prevModel.permaFrostedTokens
                            , marketCap =
                                calcMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                                    prevModel.circSupply
                            , fullyDiluted =
                                calcFullyDilutedMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                        }
                        Cmd.none

        FetchedPermaFrostBalanceLocked fetchResult ->
            case fetchResult of
                Err error ->
                    UpdateResult
                        prevModel
                        Cmd.none

                Ok value ->
                    UpdateResult
                        { prevModel
                            | permaFrostBalanceLocked = Just value
                            , permaFrostedTokens =
                                calcPermaFrostedTokens
                                    prevModel.balancerFryBalance
                                    (Just value)
                                    prevModel.permaFrostTotalSupply
                            , circSupply =
                                calcCircSupply
                                    prevModel.currentBucketId
                                    prevModel.teamTokenBalances
                                    prevModel.permaFrostedTokens
                            , marketCap =
                                calcMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                                    prevModel.circSupply
                            , fullyDiluted =
                                calcFullyDilutedMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                        }
                        Cmd.none

        FetchedPermaFrostTotalSupply fetchResult ->
            case fetchResult of
                Err error ->
                    UpdateResult
                        prevModel
                        Cmd.none

                Ok value ->
                    UpdateResult
                        { prevModel
                            | permaFrostTotalSupply = Just value
                            , permaFrostedTokens =
                                calcPermaFrostedTokens
                                    prevModel.balancerFryBalance
                                    prevModel.permaFrostBalanceLocked
                                    (Just value)
                            , circSupply =
                                calcCircSupply
                                    prevModel.currentBucketId
                                    prevModel.teamTokenBalances
                                    prevModel.permaFrostedTokens
                            , marketCap =
                                calcMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                                    prevModel.circSupply
                            , fullyDiluted =
                                calcFullyDilutedMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                        }
                        Cmd.none

        BucketValueEnteredFetched bucketId fetchResult ->
            case fetchResult of
                Err httpErr ->
                    UpdateResult
                        prevModel
                        Cmd.none

                Ok valueEntered ->
                    UpdateResult
                        { prevModel
                            | currentBucketTotalEntered =
                                Just
                                    valueEntered
                        }
                        Cmd.none

        FetchedTreasuryBalance fetchResult ->
            case fetchResult of
                Err httpErr ->
                    UpdateResult
                        prevModel
                        Cmd.none

                Ok valueFetched ->
                    UpdateResult
                        { prevModel
                            | treasuryBalance =
                                calcTreasuryBalance
                                    prevModel.currentDaiPriceEth
                                    prevModel.currentEthPriceUsd
                                    (Just valueFetched)
                        }
                        Cmd.none

        Tick i ->
            let
                getTotalValueEntered =
                    fetchTotalValueEnteredCmd prevModel.currentBucketId

                getEthPrice =
                    fetchEthPrice

                getDaiPrice =
                    fetchDaiPrice

                getFryPrice =
                    fetchFryPrice

                getTeamToast1 =
                    fetchTeamTokenBalance
                        Config.fryContractAddress
                        Config.teamToastAddress1
                        0

                getTeamToast2 =
                    fetchTeamTokenBalance
                        Config.fryContractAddress
                        Config.teamToastAddress2
                        1

                getTeamToast3 =
                    fetchTeamTokenBalance
                        Config.fryContractAddress
                        Config.teamToastAddress3
                        2

                getPermaFrostTokenBalance =
                    fetchPermaFrostLockedTokenBalance

                getPermaFrostTotalSupply =
                    fetchPermaFrostTotalSupply

                getBalancerFryBalance =
                    fetchBalancerPoolFryBalance

                getTreasuryBalance =
                    fetchTreasuryBalance
            in
            UpdateResult
                { prevModel
                    | currentTime = Time.posixToMillis i
                    , currentBucketId = getCurrentBucketId <| Time.posixToMillis i
                }
                (Cmd.batch
                    [ getTotalValueEntered
                    , getEthPrice
                    , getDaiPrice
                    , getFryPrice
                    , getTeamToast1
                    , getTeamToast2
                    , getTeamToast3
                    , getPermaFrostTokenBalance
                    , getPermaFrostTotalSupply
                    , getBalancerFryBalance
                    , getTreasuryBalance
                    ]
                )

        UpdateNow newNow ->
            if calcTimeLeft newNow <= 0 then
                UpdateResult
                    prevModel
                    Cmd.none

            else
                UpdateResult
                    { prevModel | now = newNow }
                    Cmd.none

        AmountInputChanged newInput ->
            case prevModel.depositWithdrawUXModel of
                Just ( depositOrWithdraw, amountInputUXModel ) ->
                    UpdateResult
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
                    UpdateResult prevModel

        UXBack ->
            UpdateResult
                { prevModel
                    | depositWithdrawUXModel = Nothing
                }

        DoUnlock ->
            UpdateResult
                prevModel
                doApproveChainCmdFarm

        StartDeposit defaultValue ->
            UpdateResult
                { prevModel
                    | depositWithdrawUXModel = Just ( Deposit, { amountInput = TokenValue.toFloatString Nothing defaultValue } )
                }

        StartWithdraw defaultValue ->
            UpdateResult
                { prevModel
                    | depositWithdrawUXModel = Just ( Withdraw, { amountInput = TokenValue.toFloatString Nothing defaultValue } )
                }

        DoExit ->
            UpdateResult
                prevModel
                doExitChainCmdFarm

        DoClaimRewards ->
            UpdateResult
                prevModel
                doClaimRewardsFarm

        DoDeposit amount ->
            UpdateResult
                prevModel
                [ doDepositChainCmdFarm amount
                , GTag <|
                    GTagData
                        "Deposit Liquidity"
                        "funnel"
                        ""
                        (TokenValue.mul 100 amount
                            |> TokenValue.toFloatWithWarning
                            |> floor
                        )
                ]
                |> Cmd.batch

        DoWithdraw amount ->
            UpdateResult
                prevModel
                (doWithdrawChainCmdFarm amount)

        DepositOrWithdrawSigned depositOrWithdraw amount signResult ->
            case signResult of
                Ok txHash ->
                    UpdateResult
                        { prevModel
                            | depositWithdrawUXModel = Nothing
                        }
                        (case depositOrWithdraw of
                            Deposit ->
                                GTag <|
                                    GTagData
                                        "Deposit Liquidity Signed"
                                        "conversion"
                                        (Eth.Utils.txHashToString txHash)
                                        (TokenValue.mul 100 amount
                                            |> TokenValue.toFloatWithWarning
                                            |> floor
                                        )

                            Withdraw ->
                                Cmd.none
                        )

                _ ->
                    UpdateResult prevModel

        RefetchStakingInfoOrApy ->
            if calcTimeLeft prevModel.now <= 0 then
                UpdateResult
                    prevModel
                    Cmd.none

            else
                UpdateResult
                    prevModel
                    (fetchStakingInfoOrApyCmd prevModel.now prevModel.wallet)

        StakingInfoFetched fetchResult ->
            case fetchResult of
                Err httpErr ->
                    UpdateResult
                        prevModel
                        (AddUserNotice <| web3FetchError "staking info" httpErr)

                Ok ( userStakingInfo, apy ) ->
                    UpdateResult
                        { prevModel
                            | userStakingInfo = Just userStakingInfo
                            , apy = Just apy
                        }

        ApyFetched fetchResult ->
            if calcTimeLeft prevModel.now <= 0 then
                UpdateResult prevModel

            else
                case fetchResult of
                    Err httpErr ->
                        UpdateResult
                            prevModel
                            (web3FetchError "apy" httpErr
                                |> AddUserNotice
                            )

                    Ok apy ->
                        UpdateResult
                            { prevModel
                                | apy =
                                    Just apy
                            }
                            Cmd.none

        VerifyJurisdictionClicked ->
            UpdateResult
                { prevModel
                    | jurisdictionCheckStatus = Checking
                }
                [ beginLocationCheck ()
                , gTag
                    "3a - verify jurisdiction clicked"
                    "funnel"
                    ""
                    0
                ]
                |> Cmd.batch

        LocationCheckResult decodeResult ->
            let
                jurisdictionCheckStatus =
                    locationCheckResultToJurisdictionStatus decodeResult
            in
            UpdateResult
                { prevModel
                    | jurisdictionCheckStatus = jurisdictionCheckStatus
                }
                (case jurisdictionCheckStatus of
                    WaitingForClick ->
                        Cmd.none

                    Checking ->
                        Cmd.none

                    Checked ForbiddenJurisdictions ->
                        gTag
                            "jurisdiction not allowed"
                            "funnel abort"
                            ""
                            0

                    Checked _ ->
                        gTag
                            "3b - jurisdiction verified"
                            "funnel"
                            ""
                            0

                    Error error ->
                        gTag
                            "failed jursidiction check"
                            "funnel abort"
                            error
                            0
                )

        RefreshAll ->
            UpdateResult
                prevModel
                (Cmd.batch
                    [ refreshPollVotesCmd Nothing
                    , fetchFryBalancesCmd (prevModel.fryBalances |> AddressDict.keys)
                    ]
                )

        PollsFetched pollsFetchedResult ->
            case pollsFetchedResult of
                Err httpErr ->
                    UpdateResult
                        prevModel
                        (AddUserNotice <| UN.httpFetchError "fetch polls" httpErr)

                Ok polls ->
                    UpdateResult
                        { prevModel
                            | polls = Just polls
                        }
                        (refreshPollVotesCmd Nothing)

        OptionClicked userInfo poll maybePollOptionId ->
            case userInfo of
                Nothing ->
                    UpdateResult
                        prevModel
                        Cmd.none

                Just val ->
                    UpdateResult
                        prevModel
                        (signResponseCmd val poll maybePollOptionId)

        Web3SignResultValue jsonVal ->
            let
                decodedSignResult =
                    Json.Decode.decodeValue signedResponseFromJSDecoder jsonVal
            in
            case decodedSignResult of
                Ok signResult ->
                    UpdateResult
                        prevModel
                        (sendSignedResponseCmd signResult)

                Err errStr ->
                    UpdateResult
                        prevModel
                        (AddUserNotice <| signingError <| Json.Decode.errorToString errStr)

        Web3ValidateSigResultValue jsonVal ->
            let
                decodedSignResult =
                    Json.Decode.decodeValue validateSigResultDecoder jsonVal
            in
            case decodedSignResult of
                Ok ( responseId, validateResult ) ->
                    let
                        ( newValidatedResponses, maybeRespondingAddress, maybeUserNotice ) =
                            case validateResult of
                                Valid ->
                                    let
                                        maybeSignedResponse =
                                            prevModel.maybeValidResponses
                                                |> Dict.get responseId
                                                |> Maybe.map Tuple.second
                                    in
                                    case maybeSignedResponse of
                                        Just signedResponse ->
                                            ( prevModel.validatedResponses
                                                |> insertValidatedResponse ( responseId, signedResponse )
                                            , Just signedResponse.address
                                            , Nothing
                                            )

                                        Nothing ->
                                            ( prevModel.validatedResponses
                                            , Nothing
                                            , Just <| unexpectedError "got a signature verify result from JS, but for response that I don't have!" responseId
                                            )

                                Invalid ->
                                    ( prevModel.validatedResponses
                                    , Nothing
                                    , Nothing
                                    )

                        msgsUp =
                            maybeUserNotice
                                |> Maybe.map AddUserNotice
                                |> List.singleton
                                |> Maybe.Extra.values

                        newBalancesDict =
                            case maybeRespondingAddress of
                                Nothing ->
                                    prevModel.fryBalances

                                Just address ->
                                    let
                                        newDictPortion =
                                            [ ( address
                                              , Nothing
                                              )
                                            ]
                                                |> AddressDict.fromList
                                    in
                                    AddressDict.union
                                        prevModel.fryBalances
                                        newDictPortion

                        cmd =
                            newBalancesDict
                                |> AddressDict.filter
                                    (\addressString maybeBalance ->
                                        maybeBalance == Nothing
                                    )
                                |> AddressDict.keys
                                |> fetchFryBalancesCmd
                    in
                    UpdateResult
                        { prevModel
                            | validatedResponses = newValidatedResponses
                            , maybeValidResponses =
                                prevModel.maybeValidResponses
                                    |> Dict.update responseId
                                        (Maybe.map
                                            (Tuple.mapFirst
                                                (always True)
                                            )
                                        )
                            , fryBalances = newBalancesDict
                        }
                        (cmd
                            ++ msgsUp
                        )

                Err errStr ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        [ AddUserNotice <| UN.unexpectedError "error decoding signature validation from web3js" errStr
                        ]

        ResponseSent pollId sendResult ->
            case sendResult of
                Ok _ ->
                    UpdateResult
                        prevModel
                        (refreshPollVotesCmd <| Just pollId)

                Err httpErr ->
                    UpdateResult
                        prevModel
                        (AddUserNotice <|
                            UN.httpSendError "send response" httpErr
                        )

        SignedResponsesFetched responsesFetchedResult ->
            case responsesFetchedResult of
                Ok decodedLoggedSignedResponses ->
                    case prevModel.polls of
                        Nothing ->
                            UpdateResult
                                prevModel
                                (AddUserNotice <| UN.unexpectedError "Responses were fetched, but the polls haven't loaded yet!" Nothing)

                        Just polls ->
                            let
                                newMaybeValidResponses =
                                    Dict.union
                                        prevModel.maybeValidResponses
                                        (decodedLoggedSignedResponses
                                            |> Dict.map
                                                (\_ signedResponse ->
                                                    ( False, signedResponse )
                                                )
                                        )

                                responsesToValidate =
                                    newMaybeValidResponses
                                        |> Dict.Extra.filterMap
                                            (\_ ( isValidated, signedResponse ) ->
                                                if not isValidated then
                                                    Just signedResponse

                                                else
                                                    Nothing
                                            )
                                        |> Dict.toList
                                        |> List.map (loggedSignedResponseToResponseToValidate polls)
                                        |> Maybe.Extra.values
                            in
                            UpdateResult
                                { prevModel
                                    | maybeValidResponses = newMaybeValidResponses
                                }
                                (validateSignedResponsesCmd responsesToValidate)

                Err decodeErr ->
                    UpdateResult
                        prevModel
                        (AddUserNotice <| UN.unexpectedError "error decoding responses from server" decodeErr)

        FryBalancesFetched fetchResult ->
            case fetchResult of
                Ok newFryBalances ->
                    UpdateResult
                        { prevModel
                            | fryBalances =
                                AddressDict.union
                                    (newFryBalances
                                        |> AddressDict.map (always Just)
                                    )
                                    prevModel.fryBalances
                        }

                Err httpErr ->
                    UpdateResult
                        prevModel
                        (AddUserNotice <| UN.web3FetchError "fetch polls" httpErr)

        SetMouseoverState newState ->
            UpdateResult
                { prevModel
                    | mouseoverState = newState
                }

        DepositAmountChanged amount ->
            UpdateResult
                { prevModel
                    | depositAmount = amount
                }
                (fetchIssuanceDetail
                    amount
                )

        WithdrawalAmountChanged amount ->
            UpdateResult
                { prevModel
                    | withDrawalAmount = amount
                }
                Cmd.none

        DepositClicked amount ->
            UpdateResult
                prevModel
                ((case userInfo prevModel.wallet of
                    Nothing ->
                        []

                    Just uInfo ->
                        [ doDepositChainCmd
                            uInfo.address
                            amount
                        ]
                 )
                    ++ [ Common.Msg.GTag <|
                            GTagData
                                "Squander ETH"
                                "funnel"
                                ""
                                (TokenValue.mul 100 amount
                                    |> TokenValue.toFloatWithWarning
                                    |> floor
                                )
                       ]
                )

        WithdrawClicked amount ->
            let
                userInfo =
                    prevModel.wallet
                        |> userInfo
            in
            UpdateResult
                prevModel
                (case userInfo prevModel.wallet of
                    Nothing ->
                        Cmd.none

                    Just uInfo ->
                        doWithdrawChainCmd
                            uInfo.address
                            amount
                )

        UserEthBalanceFetched fetchResult ->
            case fetchResult of
                Err _ ->
                    UpdateResult
                        prevModel
                        Cmd.none

                Ok tokenValue ->
                    UpdateResult
                        { prevModel
                            | userDerivedEthInfo =
                                (case prevModel.userDerivedEthInfo of
                                    Nothing ->
                                        { ethBalance = tokenValue
                                        , dEthBalance = TokenValue.zero
                                        , totalCollateralRedeemed = TokenValue.zero
                                        , redeemFee = TokenValue.zero
                                        , collateralReturned = TokenValue.zero
                                        , dEthAllowance = TokenValue.zero
                                        , actualCollateralAdded = TokenValue.zero
                                        , depositFee = TokenValue.zero
                                        , tokensIssued = TokenValue.zero
                                        }

                                    Just oldUserDerivedEthInfoModel ->
                                        { oldUserDerivedEthInfoModel
                                            | ethBalance = tokenValue
                                        }
                                )
                                    |> Just
                        }
                        Cmd.none

        UserDerivedEthBalanceFetched fetchResult ->
            case fetchResult of
                Err _ ->
                    UpdateResult
                        prevModel
                        Cmd.none

                Ok tokenValue ->
                    UpdateResult
                        { prevModel
                            | userDerivedEthInfo =
                                (case prevModel.userDerivedEthInfo of
                                    Nothing ->
                                        { ethBalance = TokenValue.zero
                                        , dEthBalance = tokenValue
                                        , totalCollateralRedeemed = TokenValue.zero
                                        , redeemFee = TokenValue.zero
                                        , collateralReturned = TokenValue.zero
                                        , dEthAllowance = TokenValue.zero
                                        , actualCollateralAdded = TokenValue.zero
                                        , depositFee = TokenValue.zero
                                        , tokensIssued = TokenValue.zero
                                        }

                                    Just oldUserDerivedEthInfoModel ->
                                        { oldUserDerivedEthInfoModel
                                            | dEthBalance = tokenValue
                                        }
                                )
                                    |> Just
                        }
                        Cmd.none

        DerivedEthRedeemableFetched fetchResult ->
            case fetchResult of
                Err _ ->
                    UpdateResult
                        prevModel
                        Cmd.none

                Ok ( totalCollateral, fee, returnedCollateral ) ->
                    UpdateResult
                        { prevModel
                            | userDerivedEthInfo =
                                (case prevModel.userDerivedEthInfo of
                                    Nothing ->
                                        { ethBalance = TokenValue.zero
                                        , dEthBalance = TokenValue.zero
                                        , totalCollateralRedeemed = totalCollateral
                                        , redeemFee = fee
                                        , collateralReturned = returnedCollateral
                                        , dEthAllowance = TokenValue.zero
                                        , actualCollateralAdded = TokenValue.zero
                                        , depositFee = TokenValue.zero
                                        , tokensIssued = TokenValue.zero
                                        }

                                    Just oldUserDerivedEthInfoModel ->
                                        { oldUserDerivedEthInfoModel
                                            | totalCollateralRedeemed = totalCollateral
                                            , redeemFee = fee
                                            , collateralReturned = returnedCollateral
                                        }
                                )
                                    |> Just
                        }
                        Cmd.none

        VerifyJurisdictionClicked ->
            UpdateResult
                { prevModel
                    | jurisdictionCheckStatus = Checking
                }
                [ beginLocationCheck ()
                , gTag
                    "3a - verify jurisdiction clicked"
                    "funnel"
                    ""
                    0
                ]
                |> Cmd.batch

        LocationCheckResult decodeResult ->
            let
                jurisdictionCheckStatus =
                    locationCheckResultToJurisdictionStatus decodeResult
            in
            UpdateResult
                { prevModel
                    | jurisdictionCheckStatus = jurisdictionCheckStatus
                }
                (case jurisdictionCheckStatus of
                    WaitingForClick ->
                        Cmd.none

                    Checking ->
                        Cmd.none

                    Checked ForbiddenJurisdictions ->
                        gTag
                            "jurisdiction not allowed"
                            "funnel abort"
                            ""
                            0

                    Checked _ ->
                        gTag
                            "3b - jurisdiction verified"
                            "funnel"
                            ""
                            0

                    Error error ->
                        gTag
                            "failed jursidiction check"
                            "funnel abort"
                            error
                            0
                )

        ApproveTokenSpend ->
            UpdateResult
                prevModel
                Cmd.none

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
                        (gTag
                            "Squander ETH Signed"
                            "conversion"
                            (Eth.Utils.txHashToString txHash)
                            (amount
                                |> TokenValue.mul 100
                                |> TokenValue.toFloatWithWarning
                                |> floor
                            )
                        )

                _ ->
                    UpdateResult
                        prevModel
                        Cmd.none

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
                        (gTag
                            "Redeem worthless beans Signed"
                            "conversion"
                            (Eth.Utils.txHashToString txHash)
                            (amount
                                |> TokenValue.mul 100
                                |> TokenValue.toFloatWithWarning
                                |> floor
                            )
                        )

                _ ->
                    UpdateResult
                        prevModel
                        Cmd.none

        FetchUserEthBalance ->
            UpdateResult
                prevModel
                (prevModel.wallet
                    |> fetchEthBalance
                )

        FetchUserDerivedEthBalance ->
            UpdateResult
                prevModel
                (prevModel.wallet
                    |> fetchDerivedEthBalance
                )

        DerivedEthIssuanceDetailFetched fetchResult ->
            case fetchResult of
                Err _ ->
                    UpdateResult
                        prevModel
                        Cmd.none

                Ok ( actualCollateralAdded, depositFee, tokensIssued ) ->
                    UpdateResult
                        { prevModel
                            | userDerivedEthInfo =
                                (case prevModel.userDerivedEthInfo of
                                    Nothing ->
                                        { ethBalance = TokenValue.zero
                                        , dEthBalance = TokenValue.zero
                                        , totalCollateralRedeemed = TokenValue.zero
                                        , redeemFee = TokenValue.zero
                                        , collateralReturned = TokenValue.zero
                                        , dEthAllowance = TokenValue.zero
                                        , actualCollateralAdded = actualCollateralAdded
                                        , depositFee = depositFee
                                        , tokensIssued = tokensIssued
                                        }

                                    Just oldUserDerivedEthInfoModel ->
                                        { oldUserDerivedEthInfoModel
                                            | actualCollateralAdded = actualCollateralAdded
                                            , depositFee = depositFee
                                            , tokensIssued = tokensIssued
                                        }
                                )
                                    |> Just
                        }
                        Cmd.none

        Tick _ ->
            UpdateResult
                prevModel
                ((case userInfo prevModel.wallet of
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

        GotoRoute route ->
            prevModel
                |> Misc.gotoRoute route
                |> Tuple.mapSecond
                    (\cmd ->
                        Cmd.batch
                            [ cmd
                            , Browser.Navigation.pushUrl
                                prevModel.navKey
                                (Routing.routeToString prevModel.basePath route)
                            ]
                    )

        ConnectToWeb3 ->
            case prevModel.wallet of
                Types.NoneDetected ->
                    ( prevModel |> addUserNotice cantConnectNoWeb3
                    , Cmd.none
                    )

                _ ->
                    ( prevModel
                    , Ports.connectToWeb3 ()
                    )

        ShowOrHideAddress phaceId ->
            ( { prevModel
                | showAddressId =
                    if prevModel.showAddressId == Just phaceId then
                        Nothing

                    else
                        Just phaceId
              }
            , Cmd.none
            )

        AddUserNotice userNotice ->
            ( prevModel |> addUserNotice userNotice
            , Cmd.none
            )

        GTag gtag ->
            ( prevModel
            , Ports.gTagOut (encodeGTag gtag)
            )

        NonRepeatingGTag gtag ->
            let
                alreadySent =
                    prevModel.nonRepeatingGTagsSent
                        |> List.any
                            (\event ->
                                event == gtag.event
                            )
            in
            if alreadySent then
                ( prevModel, Cmd.none )

            else
                ( { prevModel
                    | nonRepeatingGTagsSent =
                        prevModel.nonRepeatingGTagsSent
                            |> List.append
                                [ gtag.event ]
                  }
                , Ports.gTagOut (encodeGTag gtag)
                )

        Types.NoOp ->
            ( prevModel, Cmd.none )
