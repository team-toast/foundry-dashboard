module Update exposing (..)

import AddressDict exposing (AddressDict)
import Array exposing (Array)
import Browser
import Browser.Navigation
import Config
import Dict
import Dict.Extra
import ElementHelpers as EH exposing (DisplayProfile(..))
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Sentry.Tx as TxSentry exposing (TxSentry)
import Eth.Utils
import GTag exposing (GTagData, gTagOut)
import Helpers.Tuple exposing (tuple3MapSecond, tuple3Second, tuple3ToList)
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
import UserNotice exposing (UserNotice, cantConnectNoWeb3, httpFetchError, httpSendError, routeNotFound, signingError, unexpectedError, walletError, web3FetchError)
import UserTx exposing (Initiator)


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
            case Routing.urlToRoute url of
                Routing.Home ->
                    ( { prevModel
                        | route = Routing.Home
                      }
                    , Cmd.none
                    )

                Routing.Sentiment ->
                    ( { prevModel
                        | route = Routing.Sentiment
                      }
                    , Cmd.none
                    )

                Routing.Stats ->
                    ( { prevModel
                        | route = Routing.Stats
                      }
                    , Cmd.none
                    )

                Routing.Farm ->
                    ( { prevModel
                        | route = Routing.Farm
                      }
                    , Cmd.none
                    )

                Routing.DerivedEth ->
                    ( { prevModel
                        | route = Routing.DerivedEth
                      }
                    , Cmd.none
                    )

                Routing.NotFound err ->
                    ( { prevModel
                        | route = Routing.NotFound "url not found"
                      }
                        |> addUserNotice routeNotFound
                    , Cmd.none
                    )

        Navigate route ->
            case route of
                Routing.Home ->
                    ( { prevModel
                        | route = Routing.Stats
                      }
                    , Browser.Navigation.pushUrl
                        prevModel.navKey
                        (Routing.routeToString prevModel.basePath Routing.Stats)
                    )

                Routing.Sentiment ->
                    ( { prevModel
                        | route = Routing.Sentiment
                      }
                    , Browser.Navigation.pushUrl
                        prevModel.navKey
                        (Routing.routeToString prevModel.basePath Routing.Sentiment)
                    )

                Routing.Stats ->
                    ( { prevModel
                        | route = Routing.Stats
                      }
                    , Browser.Navigation.pushUrl
                        prevModel.navKey
                        (Routing.routeToString prevModel.basePath Routing.Stats)
                    )

                Routing.Farm ->
                    ( { prevModel
                        | route = Routing.Farm
                      }
                    , Browser.Navigation.pushUrl
                        prevModel.navKey
                        (Routing.routeToString prevModel.basePath Routing.Farm)
                    )

                Routing.DerivedEth ->
                    ( { prevModel
                        | route = Routing.DerivedEth
                      }
                    , Browser.Navigation.pushUrl
                        prevModel.navKey
                        (Routing.routeToString prevModel.basePath Routing.DerivedEth)
                    )

                Routing.NotFound err ->
                    ( { prevModel
                        | route = Routing.NotFound "url not found"
                      }
                        |> addUserNotice routeNotFound
                    , Cmd.none
                    )

        Tick i ->
            ( { prevModel
                | now = i
                , currentTime = Time.posixToMillis i
                , currentBucketId = getCurrentBucketId <| Time.posixToMillis i
              }
            , [ fetchTotalValueEnteredCmd prevModel.currentBucketId
              , fetchEthPrice
              , fetchDaiPrice
              , fetchFryPrice
              , fetchTeamTokenBalance Config.fryContractAddress Config.teamToastAddress1 0
              , fetchTeamTokenBalance Config.fryContractAddress Config.teamToastAddress2 1
              , fetchTeamTokenBalance Config.fryContractAddress Config.teamToastAddress3 2
              , fetchPermaFrostLockedTokenBalance
              , fetchPermaFrostTotalSupply
              , fetchBalancerPoolFryBalance
              , fetchTreasuryBalance
              , prevModel.wallet
                    |> fetchDerivedEthBalance
              , prevModel.wallet
                    |> fetchEthBalance
              , prevModel.withDrawalAmount
                    |> TokenValue.fromString
                    |> fetchDethPositionInfo
              ]
                |> Cmd.batch
            )

        Resize width _ ->
            ( { prevModel
                | dProfile =
                    EH.screenWidthToDisplayProfile 1280 width
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
                    ( { prevModel
                        | wallet = newWallet
                      }
                    , [ fetchDerivedEthBalance newWallet
                      , fetchEthBalance newWallet
                      ]
                        |> Cmd.batch
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
                , gTagOut <|
                    GTagData
                        "accept cookies"
                        Nothing
                        Nothing
                        Nothing
                ]
            )

        ClickHappened ->
            ( { prevModel
                | showAddressId = Nothing
              }
            , Cmd.none
            )

        NoOp ->
            ( prevModel, Cmd.none )

        FetchedEthPrice fetchResult ->
            case fetchResult of
                Err error ->
                    ( prevModel
                    , Cmd.none
                    )

                Ok bundle1 ->
                    let
                        v =
                            case bundle1 of
                                Just val ->
                                    val

                                Nothing ->
                                    Value 0
                    in
                    ( { prevModel
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
                    , Cmd.none
                    )

        FetchedDaiPrice fetchResult ->
            case fetchResult of
                Err error ->
                    ( prevModel
                    , Cmd.none
                    )

                Ok value ->
                    let
                        v =
                            case value of
                                Just val ->
                                    val

                                Nothing ->
                                    Value 0
                    in
                    ( { prevModel
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
                    , Cmd.none
                    )

        FetchedFryPrice fetchResult ->
            case fetchResult of
                Err error ->
                    ( prevModel
                    , Cmd.none
                    )

                Ok value ->
                    let
                        v =
                            case value of
                                Just val ->
                                    val

                                Nothing ->
                                    Value 0
                    in
                    ( { prevModel
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
                    , Cmd.none
                    )

        FetchedTeamTokens index fetchResult ->
            case fetchResult of
                Err error ->
                    ( prevModel
                    , Cmd.none
                    )

                Ok value ->
                    ( { prevModel
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
                    , Cmd.none
                    )

        FetchedBalancerFryBalance fetchResult ->
            case fetchResult of
                Err error ->
                    ( prevModel
                    , Cmd.none
                    )

                Ok value ->
                    ( { prevModel
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
                    , Cmd.none
                    )

        FetchedPermaFrostBalanceLocked fetchResult ->
            case fetchResult of
                Err error ->
                    ( prevModel
                    , Cmd.none
                    )

                Ok value ->
                    ( { prevModel
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
                    , Cmd.none
                    )

        FetchedPermaFrostTotalSupply fetchResult ->
            case fetchResult of
                Err error ->
                    ( prevModel
                    , Cmd.none
                    )

                Ok value ->
                    ( { prevModel
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
                    , Cmd.none
                    )

        BucketValueEnteredFetched bucketId fetchResult ->
            case fetchResult of
                Err httpErr ->
                    ( prevModel
                    , Cmd.none
                    )

                Ok valueEntered ->
                    ( { prevModel
                        | currentBucketTotalEntered =
                            Just
                                valueEntered
                      }
                    , Cmd.none
                    )

        FetchedTreasuryBalance fetchResult ->
            case fetchResult of
                Err httpErr ->
                    ( prevModel
                    , Cmd.none
                    )

                Ok valueFetched ->
                    ( { prevModel
                        | treasuryBalance =
                            calcTreasuryBalance
                                prevModel.currentDaiPriceEth
                                prevModel.currentEthPriceUsd
                                (Just valueFetched)
                      }
                    , Cmd.none
                    )

        UpdateNow newNow ->
            if calcTimeLeft newNow <= 0 then
                ( prevModel
                , Cmd.none
                )

            else
                ( { prevModel | now = newNow }
                , Cmd.none
                )

        AmountInputChanged newInput ->
            case prevModel.depositWithdrawUXModel of
                Just ( depositOrWithdraw, amountInputUXModel ) ->
                    ( { prevModel
                        | depositWithdrawUXModel =
                            Just
                                ( depositOrWithdraw
                                , { amountInputUXModel
                                    | amountInput = newInput
                                  }
                                )
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( prevModel, Cmd.none )

        UXBack ->
            ( { prevModel
                | depositWithdrawUXModel = Nothing
              }
            , Cmd.none
            )

        DoUnlock ->
            ( prevModel
            , doApproveChainCmdFarm
                |> attemptTxInitiate prevModel.txSentry prevModel.trackedTxs
            )

        StartDeposit defaultValue ->
            ( { prevModel
                | depositWithdrawUXModel = Just ( Deposit, { amountInput = TokenValue.toFloatString Nothing defaultValue } )
              }
            , Cmd.none
            )

        StartWithdraw defaultValue ->
            ( { prevModel
                | depositWithdrawUXModel = Just ( Withdraw, { amountInput = TokenValue.toFloatString Nothing defaultValue } )
              }
            , Cmd.none
            )

        DoExit ->
            ( prevModel
            , doExitChainCmdFarm
                |> attemptTxInitiate prevModel.txSentry prevModel.trackedTxs
            )

        DoClaimRewards ->
            ( prevModel
            , doClaimRewardsFarm
                |> attemptTxInitiate prevModel.txSentry prevModel.trackedTxs
            )

        DoDeposit amount ->
            ( prevModel
            , [ doDepositChainCmdFarm amount
                    |> attemptTxInitiate prevModel.txSentry prevModel.trackedTxs
              , gTagOut <|
                    GTagData
                        "Deposit Liquidity"
                        (Just "funnel")
                        Nothing
                        (Just
                            (TokenValue.mul amount 100
                                |> TokenValue.toFloatWithWarning
                                |> floor
                            )
                        )
              ]
                |> Cmd.batch
            )

        DoWithdraw amount ->
            ( prevModel
            , doWithdrawChainCmdFarm amount
                |> attemptTxInitiate prevModel.txSentry prevModel.trackedTxs
            )

        DepositOrWithdrawSigned depositOrWithdraw amount signResult ->
            case signResult of
                Ok txHash ->
                    ( { prevModel
                        | depositWithdrawUXModel = Nothing
                      }
                    , case depositOrWithdraw of
                        Deposit ->
                            gTagOut <|
                                GTagData
                                    "Deposit Liquidity Signed"
                                    (Just "conversion")
                                    (Just <| Eth.Utils.txHashToString txHash)
                                    (Just
                                        (TokenValue.mul amount 100
                                            |> TokenValue.toFloatWithWarning
                                            |> floor
                                        )
                                    )

                        Withdraw ->
                            Cmd.none
                    )

                _ ->
                    ( prevModel, Cmd.none )

        RefetchStakingInfoOrApy ->
            ( prevModel
            , fetchStakingInfoOrApyCmd prevModel.now prevModel.wallet
            )

        StakingInfoFetched fetchResult ->
            case fetchResult of
                Err httpErr ->
                    ( prevModel
                        |> (web3FetchError "staking info" httpErr
                                |> addUserNotice
                           )
                    , Cmd.none
                    )

                Ok ( userStakingInfo, apy ) ->
                    ( { prevModel
                        | userStakingInfo = Just userStakingInfo
                        , apy = Just apy
                      }
                    , Cmd.none
                    )

        ApyFetched fetchResult ->
            case fetchResult of
                Err httpErr ->
                    ( prevModel
                        |> (web3FetchError "apy" httpErr
                                |> addUserNotice
                           )
                    , Cmd.none
                    )

                Ok apy ->
                    ( { prevModel
                        | apy =
                            Just apy
                      }
                    , Cmd.none
                    )

        VerifyJurisdictionClicked ->
            ( { prevModel
                | jurisdictionCheckStatus = Checking
              }
            , [ Ports.beginLocationCheck ()
              , gTagOut <|
                    GTagData
                        "3a - verify jurisdiction clicked"
                        (Just "funnel")
                        Nothing
                        Nothing
              ]
                |> Cmd.batch
            )

        LocationCheckResult decodeResult ->
            let
                jurisdictionCheckStatus =
                    locationCheckResultToJurisdictionStatus decodeResult
            in
            ( { prevModel
                | jurisdictionCheckStatus = jurisdictionCheckStatus
              }
            , case jurisdictionCheckStatus of
                WaitingForClick ->
                    Cmd.none

                Checking ->
                    Cmd.none

                Checked ForbiddenJurisdictions ->
                    gTagOut <|
                        GTagData
                            "jurisdiction not allowed"
                            (Just "funnel abort")
                            Nothing
                            Nothing

                Checked _ ->
                    gTagOut <|
                        GTagData
                            "3b - jurisdiction verified"
                            (Just "funnel")
                            Nothing
                            Nothing

                Error error ->
                    gTagOut <|
                        GTagData
                            "failed jursidiction check"
                            (Just "funnel abort")
                            Nothing
                            Nothing
            )

        RefreshAll ->
            ( prevModel
            , Cmd.batch
                [ refreshPollVotesCmd Nothing
                , fetchFryBalancesCmd (prevModel.fryBalances |> AddressDict.keys)
                ]
            )

        PollsFetched pollsFetchedResult ->
            case pollsFetchedResult of
                Err httpErr ->
                    ( prevModel
                        |> (httpFetchError "fetch polls" httpErr |> addUserNotice)
                    , Cmd.none
                    )

                Ok polls ->
                    ( { prevModel
                        | polls = Just polls
                      }
                    , refreshPollVotesCmd Nothing
                    )

        OptionClicked userInfo poll maybePollOptionId ->
            case userInfo of
                Nothing ->
                    ( prevModel
                    , Cmd.none
                    )

                Just val ->
                    ( prevModel
                    , signResponseCmd val poll maybePollOptionId
                    )

        Web3SignResultValue jsonVal ->
            let
                decodedSignResult =
                    Json.Decode.decodeValue signedResponseFromJSDecoder jsonVal
            in
            case decodedSignResult of
                Ok signResult ->
                    ( prevModel
                    , sendSignedResponseCmd signResult
                    )

                Err errStr ->
                    ( prevModel
                        |> (Json.Decode.errorToString errStr
                                |> signingError
                                |> addUserNotice
                           )
                    , Cmd.none
                    )

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

                        tempModel =
                            case maybeUserNotice of
                                Nothing ->
                                    prevModel

                                Just userNotice ->
                                    prevModel
                                        |> (userNotice
                                                |> addUserNotice
                                           )
                    in
                    ( { tempModel
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
                    , cmd
                    )

                Err errStr ->
                    ( prevModel
                        |> (unexpectedError "error decoding signature validation from web3js" errStr
                                |> addUserNotice
                           )
                    , Cmd.none
                    )

        ResponseSent pollId sendResult ->
            case sendResult of
                Ok _ ->
                    ( prevModel
                    , refreshPollVotesCmd <| Just pollId
                    )

                Err httpErr ->
                    ( prevModel
                        |> (httpSendError "send response" httpErr
                                |> addUserNotice
                           )
                    , Cmd.none
                    )

        SignedResponsesFetched responsesFetchedResult ->
            case responsesFetchedResult of
                Ok decodedLoggedSignedResponses ->
                    case prevModel.polls of
                        Nothing ->
                            ( prevModel
                                |> (unexpectedError "Responses were fetched, but the polls haven't loaded yet!" Nothing
                                        |> addUserNotice
                                   )
                            , Cmd.none
                            )

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
                            ( { prevModel
                                | maybeValidResponses = newMaybeValidResponses
                              }
                            , validateSignedResponsesCmd responsesToValidate
                            )

                Err decodeErr ->
                    ( prevModel
                        |> (unexpectedError "error decoding responses from server" decodeErr
                                |> addUserNotice
                           )
                    , Cmd.none
                    )

        FryBalancesFetched fetchResult ->
            case fetchResult of
                Ok newFryBalances ->
                    ( { prevModel
                        | fryBalances =
                            AddressDict.union
                                (newFryBalances
                                    |> AddressDict.map (always Just)
                                )
                                prevModel.fryBalances
                      }
                    , Cmd.none
                    )

                Err httpErr ->
                    ( prevModel
                        |> (web3FetchError "fetch polls" httpErr
                                |> addUserNotice
                           )
                    , Cmd.none
                    )

        SetMouseoverState newState ->
            ( { prevModel
                | mouseoverState = newState
              }
            , Cmd.none
            )

        DepositAmountChanged amount ->
            ( { prevModel
                | depositAmount = amount
              }
            , fetchIssuanceDetail
                amount
            )

        WithdrawalAmountChanged amount ->
            ( { prevModel
                | withDrawalAmount = amount
              }
            , amount
                |> TokenValue.fromString
                |> fetchDethPositionInfo
            )

        DepositClicked amount ->
            ( prevModel
            , (case userInfo prevModel.wallet of
                Nothing ->
                    []

                Just uInfo ->
                    [ doDepositChainCmd
                        uInfo.address
                        amount
                        |> attemptTxInitiate prevModel.txSentry prevModel.trackedTxs
                    ]
              )
                ++ [ gTagOut <|
                        GTagData
                            "Squander ETH"
                            (Just "funnel")
                            Nothing
                            (Just
                                (TokenValue.mul amount 100
                                    |> TokenValue.toFloatWithWarning
                                    |> floor
                                )
                            )
                   ]
                |> Cmd.batch
            )

        WithdrawClicked amount ->
            ( prevModel
            , case userInfo prevModel.wallet of
                Nothing ->
                    Cmd.none

                Just uInfo ->
                    doWithdrawChainCmd
                        uInfo.address
                        amount
                        |> attemptTxInitiate prevModel.txSentry prevModel.trackedTxs
            )

        UserEthBalanceFetched fetchResult ->
            case fetchResult of
                Err _ ->
                    ( prevModel
                    , Cmd.none
                    )

                Ok tokenValue ->
                    ( { prevModel
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
                    , Cmd.none
                    )

        UserDerivedEthBalanceFetched fetchResult ->
            case fetchResult of
                Err _ ->
                    ( prevModel
                    , Cmd.none
                    )

                Ok tokenValue ->
                    ( { prevModel
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
                    , Cmd.none
                    )

        DerivedEthRedeemableFetched fetchResult ->
            case fetchResult of
                Err _ ->
                    ( prevModel
                    , Cmd.none
                    )

                Ok ( totalCollateral, fee, returnedCollateral ) ->
                    ( { prevModel
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
                    , Cmd.none
                    )

        ApproveTokenSpend ->
            ( prevModel
            , Cmd.none
            )

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
                    ( { prevModel
                        | depositAmount = ""
                      }
                    , [ gTagOut <|
                            GTagData
                                "Squander ETH Signed"
                                (Just "conversion")
                                (Just <| Eth.Utils.txHashToString txHash)
                                (Just
                                    (TokenValue.mul amount 100
                                        |> TokenValue.toFloatWithWarning
                                        |> floor
                                    )
                                )
                      ]
                        |> Cmd.batch
                    )

                _ ->
                    ( prevModel
                    , Cmd.none
                    )

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
                    ( { prevModel
                        | withDrawalAmount = ""
                      }
                    , [ gTagOut <|
                            GTagData
                                "Redeem worthless beans Signed"
                                (Just "conversion")
                                (Just <| Eth.Utils.txHashToString txHash)
                                (Just
                                    (TokenValue.mul amount 100
                                        |> TokenValue.toFloatWithWarning
                                        |> floor
                                    )
                                )
                      ]
                        |> Cmd.batch
                    )

                _ ->
                    ( prevModel
                    , Cmd.none
                    )

        FetchUserEthBalance ->
            ( prevModel
            , prevModel.wallet
                |> fetchEthBalance
            )

        FetchUserDerivedEthBalance ->
            ( prevModel
            , prevModel.wallet
                |> fetchDerivedEthBalance
            )

        DerivedEthIssuanceDetailFetched fetchResult ->
            case fetchResult of
                Err _ ->
                    ( prevModel
                    , Cmd.none
                    )

                Ok ( actualCollateralAdded, depositFee, tokensIssued ) ->
                    ( { prevModel
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
                    , Cmd.none
                    )

        GotoRoute route ->
            prevModel
                |> gotoRoute route
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
                    ( prevModel
                        |> addUserNotice cantConnectNoWeb3
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
            ( prevModel
                |> addUserNotice userNotice
            , Cmd.none
            )


gotoRoute : Routing.Route -> Model -> ( Model, Cmd Msg )
gotoRoute route prevModel =
    case route of
        Routing.Home ->
            ( { prevModel
                | route = route
              }
            , Cmd.none
            )

        Routing.Sentiment ->
            ( { prevModel
                | route = route
              }
            , Cmd.none
            )

        Routing.Stats ->
            ( { prevModel
                | route = route
              }
            , Cmd.none
            )

        Routing.Farm ->
            ( { prevModel
                | route = route
              }
            , Cmd.none
            )

        Routing.DerivedEth ->
            ( { prevModel
                | route = route
              }
            , Cmd.none
            )

        Routing.NotFound err ->
            ( { prevModel
                | route = route
              }
                |> addUserNotice routeNotFound
            , Cmd.none
            )


addUserNotice : UserNotice -> Model -> Model
addUserNotice notice model =
    model
        |> addUserNotices [ notice ]


addUserNotices : List UserNotice -> Model -> Model
addUserNotices notices model =
    { model
        | userNotices =
            List.append
                model.userNotices
                notices
                |> List.Extra.uniqueBy .uniqueLabel
    }


attemptTxInitiate : TxSentry.TxSentry Msg -> UserTx.Tracker Msg -> Initiator Msg -> Cmd Msg
attemptTxInitiate sentry trackedTxs initiator =
    initiateUserTx sentry trackedTxs initiator
        |> tuple3Second
