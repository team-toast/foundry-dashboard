module Update exposing (..)

import AddressDict exposing (AddressDict)
import Array exposing (Array)
import Browser
import Browser.Navigation
import Chain
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
import Result.Extra exposing (unpack)
import Routing
import Time
import TokenValue
import Types exposing (..)
import Url
import UserNotice as UN exposing (UserNotice, cantConnectNoWeb3, httpFetchError, httpSendError, routeNotFound, signingError, unexpectedError, walletError, web3FetchError)
import UserTx exposing (Initiator)
import Wallet


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            let
                cmd =
                    case urlRequest of
                        Browser.Internal url ->
                            Browser.Navigation.pushUrl model.navKey (Url.toString url)

                        Browser.External href ->
                            Browser.Navigation.load href
            in
            ( model, cmd )

        UrlChanged url ->
            case Routing.urlToRoute url of
                Routing.Home ->
                    ( { model
                        | route = Routing.Home
                      }
                    , Cmd.none
                    )

                Routing.Sentiment ->
                    ( { model
                        | route = Routing.Sentiment
                      }
                    , Cmd.none
                    )

                Routing.Stats ->
                    ( { model
                        | route = Routing.Stats
                      }
                    , Cmd.none
                    )

                Routing.Farm ->
                    ( { model
                        | route = Routing.Farm
                      }
                    , Cmd.none
                    )

                Routing.DerivedEth ->
                    ( { model
                        | route = Routing.DerivedEth
                      }
                    , Cmd.none
                    )

                Routing.NotFound err ->
                    ( { model
                        | route = Routing.NotFound "url not found"
                      }
                        |> addUserNotice routeNotFound
                    , Cmd.none
                    )

        Navigate route ->
            case route of
                Routing.Home ->
                    ( { model
                        | route = Routing.Stats
                      }
                    , Browser.Navigation.pushUrl
                        model.navKey
                        (Routing.routeToString model.basePath Routing.Stats)
                    )

                Routing.Sentiment ->
                    ( { model
                        | route = Routing.Sentiment
                      }
                    , Browser.Navigation.pushUrl
                        model.navKey
                        (Routing.routeToString model.basePath Routing.Sentiment)
                    )

                Routing.Stats ->
                    ( { model
                        | route = Routing.Stats
                      }
                    , Browser.Navigation.pushUrl
                        model.navKey
                        (Routing.routeToString model.basePath Routing.Stats)
                    )

                Routing.Farm ->
                    ( { model
                        | route = Routing.Farm
                      }
                    , Browser.Navigation.pushUrl
                        model.navKey
                        (Routing.routeToString model.basePath Routing.Farm)
                    )

                Routing.DerivedEth ->
                    ( { model
                        | route = Routing.DerivedEth
                      }
                    , Browser.Navigation.pushUrl
                        model.navKey
                        (Routing.routeToString model.basePath Routing.DerivedEth)
                    )

                Routing.NotFound err ->
                    ( { model
                        | route = Routing.NotFound "url not found"
                      }
                        |> addUserNotice routeNotFound
                    , Cmd.none
                    )

        Tick i ->
            let
                chain =
                    model.wallet
                        |> Wallet.userInfo
                        |> Chain.whenJust
                            (\userInfo ->
                                userInfo.chain
                            )
            in
            ( { model
                | now = i
                , currentTime = Time.posixToMillis i
                , currentBucketId = getCurrentBucketId <| Time.posixToMillis i
              }
            , [ fetchTotalValueEnteredCmd chain model.currentBucketId
              , fetchEthPrice
              , fetchDaiPrice <| chain
              , fetchFryPrice <| chain
              , fetchTeamTokenBalance chain (Config.fryContractAddress <| chain) Config.teamToastAddress1 0
              , fetchTeamTokenBalance chain (Config.fryContractAddress <| chain) Config.teamToastAddress2 1
              , fetchTeamTokenBalance chain (Config.fryContractAddress <| chain) Config.teamToastAddress3 2
              , fetchPermaFrostLockedTokenBalance <| chain
              , fetchPermaFrostTotalSupply <| chain
              , fetchBalancerPoolFryBalance <| chain
              , fetchTreasuryBalance <| chain
              , model.wallet
                    |> (fetchDerivedEthBalance <|
                            chain
                       )
              , model.wallet
                    |> (fetchEthBalance <| chain)
              , model.withDrawalAmount
                    |> TokenValue.fromString
                    |> (fetchDethPositionInfo <| chain)
              ]
                |> Cmd.batch
            )

        Resize width _ ->
            ( { model
                | dProfile =
                    EH.screenWidthToDisplayProfile 1280 width
              }
            , Cmd.none
            )

        EveryFewSeconds ->
            ( model
            , Cmd.none
            )

        WalletStatus walletSentryResult ->
            case walletSentryResult of
                Ok walletSentry ->
                    let
                        chain =
                            model.wallet
                                |> Wallet.userInfo
                                |> Chain.whenJust
                                    (\userInfo ->
                                        userInfo.chain
                                    )

                        newWallet =
                            case walletSentry.account of
                                Just newAddress ->
                                    if
                                        (model.wallet
                                            |> userInfo
                                            |> Maybe.map .address
                                        )
                                            == Just newAddress
                                    then
                                        model.wallet

                                    else
                                        UserInfo
                                            newAddress
                                            TokenValue.zero
                                            chain
                                            XDaiStandby
                                            |> Types.Active

                                Nothing ->
                                    NoneDetected
                    in
                    ( { model
                        | wallet = newWallet
                      }
                    , [ fetchDerivedEthBalance chain newWallet
                      , fetchEthBalance chain newWallet
                      ]
                        |> Cmd.batch
                    )

                Err errStr ->
                    ( model
                        |> addUserNotice (walletError errStr)
                    , Cmd.none
                    )

        TxSentryMsg subMsg ->
            let
                ( newTxSentry, subCmd ) =
                    TxSentry.update subMsg model.txSentry
            in
            ( { model | txSentry = newTxSentry }, subCmd )

        EventSentryMsg chain eventMsg ->
            case chain of
                Eth ->
                    let
                        ( newEventSentry, cmd ) =
                            EventSentry.update
                                eventMsg
                                model.sentries.ethereum
                    in
                    ( { model
                        | sentries =
                            model.sentries
                                |> (\ss ->
                                        { ss
                                            | ethereum =
                                                newEventSentry
                                        }
                                   )
                      }
                    , cmd
                    )

                BSC ->
                    let
                        ( newEventSentry, cmd ) =
                            EventSentry.update
                                eventMsg
                                model.sentries.bsc
                    in
                    ( { model
                        | sentries =
                            model.sentries
                                |> (\ss ->
                                        { ss
                                            | bsc =
                                                newEventSentry
                                        }
                                   )
                      }
                    , cmd
                    )

                XDai ->
                    let
                        ( newEventSentry, cmd ) =
                            EventSentry.update
                                eventMsg
                                model.sentries.xDai
                    in
                    ( { model
                        | sentries =
                            model.sentries
                                |> (\ss ->
                                        { ss
                                            | xDai =
                                                newEventSentry
                                        }
                                   )
                      }
                    , cmd
                    )

        DismissNotice id ->
            ( { model
                | userNotices =
                    model.userNotices |> List.Extra.removeAt id
              }
            , Cmd.none
            )

        ShowExpandedTrackedTxs flag ->
            ( { model
                | trackedTxsExpanded = flag
              }
            , Cmd.none
            )

        TxSigned trackedTxId signResult ->
            let
                newTrackedTxs =
                    model.trackedTxs
                        |> UserTx.setTrackedTxStatus trackedTxId
                            (case signResult of
                                Err errStr ->
                                    UserTx.Rejected

                                Ok txHash ->
                                    UserTx.Signed txHash UserTx.Mining
                            )

                maybeExtraMsgConstructor =
                    model.trackedTxs
                        |> List.Extra.getAt trackedTxId
                        |> Maybe.map .notifiers
                        |> Maybe.andThen .onSign

                intermediateModel =
                    { model
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
                    model.trackedTxs
                        |> UserTx.setTrackedTxSignedStatus trackedTxId
                            (case mineResult of
                                Err errStr ->
                                    UserTx.Failed

                                Ok txReceipt ->
                                    UserTx.Success txReceipt
                            )

                maybeExtraMsgConstructor =
                    model.trackedTxs
                        |> List.Extra.getAt trackedTxId
                        |> Maybe.map .notifiers
                        |> Maybe.andThen .onMine

                intermediateModel =
                    { model
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
            ( { model
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
            ( { model
                | showAddressId = Nothing
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )

        FetchedEthPrice fetchResult ->
            case fetchResult of
                Err error ->
                    ( model
                    , Cmd.none
                    )

                Ok bundle1 ->
                    let
                        v =
                            case bundle1 of
                                Just val ->
                                    val

                                Nothing ->
                                    PriceValue 0
                    in
                    ( { model
                        | currentEthPriceUsd =
                            Just
                                v.ethPrice
                        , circSupply =
                            calcCircSupply
                                model.currentBucketId
                                model.teamTokenBalances
                                model.permaFrostedTokens
                        , marketCap =
                            calcMarketCap
                                model.currentFryPriceEth
                                model.currentEthPriceUsd
                                model.circSupply
                        , fullyDiluted =
                            calcFullyDilutedMarketCap
                                model.currentFryPriceEth
                                model.currentEthPriceUsd
                      }
                    , Cmd.none
                    )

        FetchedDaiPrice fetchResult ->
            case fetchResult of
                Err error ->
                    ( model
                    , Cmd.none
                    )

                Ok value ->
                    let
                        v =
                            case value of
                                Just val ->
                                    val

                                Nothing ->
                                    PriceValue 0
                    in
                    ( { model
                        | currentDaiPriceEth =
                            Just
                                v.ethPrice
                        , circSupply =
                            calcCircSupply
                                model.currentBucketId
                                model.teamTokenBalances
                                model.permaFrostedTokens
                        , marketCap =
                            calcMarketCap
                                model.currentFryPriceEth
                                model.currentEthPriceUsd
                                model.circSupply
                        , fullyDiluted =
                            calcFullyDilutedMarketCap
                                model.currentFryPriceEth
                                model.currentEthPriceUsd
                      }
                    , Cmd.none
                    )

        FetchedFryPrice fetchResult ->
            case fetchResult of
                Err error ->
                    ( model
                    , Cmd.none
                    )

                Ok value ->
                    let
                        v =
                            case value of
                                Just val ->
                                    val

                                Nothing ->
                                    PriceValue 0
                    in
                    ( { model
                        | currentFryPriceEth =
                            Just
                                v.ethPrice
                        , circSupply =
                            calcCircSupply
                                model.currentBucketId
                                model.teamTokenBalances
                                model.permaFrostedTokens
                        , marketCap =
                            calcMarketCap
                                model.currentFryPriceEth
                                model.currentEthPriceUsd
                                model.circSupply
                        , fullyDiluted =
                            calcFullyDilutedMarketCap
                                model.currentFryPriceEth
                                model.currentEthPriceUsd
                      }
                    , Cmd.none
                    )

        FetchedTeamTokens index fetchResult ->
            case fetchResult of
                Err error ->
                    ( model
                    , Cmd.none
                    )

                Ok value ->
                    ( { model
                        | teamTokenBalances = Array.set index (Just value) model.teamTokenBalances
                        , circSupply =
                            calcCircSupply
                                model.currentBucketId
                                model.teamTokenBalances
                                model.permaFrostedTokens
                        , marketCap =
                            calcMarketCap
                                model.currentFryPriceEth
                                model.currentEthPriceUsd
                                model.circSupply
                        , fullyDiluted =
                            calcFullyDilutedMarketCap
                                model.currentFryPriceEth
                                model.currentEthPriceUsd
                      }
                    , Cmd.none
                    )

        FetchedBalancerFryBalance fetchResult ->
            case fetchResult of
                Err error ->
                    ( model
                    , Cmd.none
                    )

                Ok value ->
                    ( { model
                        | balancerFryBalance = Just value
                        , permaFrostedTokens =
                            calcPermaFrostedTokens
                                (Just value)
                                model.permaFrostBalanceLocked
                                model.permaFrostTotalSupply
                        , circSupply =
                            calcCircSupply
                                model.currentBucketId
                                model.teamTokenBalances
                                model.permaFrostedTokens
                        , marketCap =
                            calcMarketCap
                                model.currentFryPriceEth
                                model.currentEthPriceUsd
                                model.circSupply
                        , fullyDiluted =
                            calcFullyDilutedMarketCap
                                model.currentFryPriceEth
                                model.currentEthPriceUsd
                      }
                    , Cmd.none
                    )

        FetchedPermaFrostBalanceLocked fetchResult ->
            case fetchResult of
                Err error ->
                    ( model
                    , Cmd.none
                    )

                Ok value ->
                    ( { model
                        | permaFrostBalanceLocked = Just value
                        , permaFrostedTokens =
                            calcPermaFrostedTokens
                                model.balancerFryBalance
                                (Just value)
                                model.permaFrostTotalSupply
                        , circSupply =
                            calcCircSupply
                                model.currentBucketId
                                model.teamTokenBalances
                                model.permaFrostedTokens
                        , marketCap =
                            calcMarketCap
                                model.currentFryPriceEth
                                model.currentEthPriceUsd
                                model.circSupply
                        , fullyDiluted =
                            calcFullyDilutedMarketCap
                                model.currentFryPriceEth
                                model.currentEthPriceUsd
                      }
                    , Cmd.none
                    )

        FetchedPermaFrostTotalSupply fetchResult ->
            case fetchResult of
                Err error ->
                    ( model
                    , Cmd.none
                    )

                Ok value ->
                    ( { model
                        | permaFrostTotalSupply = Just value
                        , permaFrostedTokens =
                            calcPermaFrostedTokens
                                model.balancerFryBalance
                                model.permaFrostBalanceLocked
                                (Just value)
                        , circSupply =
                            calcCircSupply
                                model.currentBucketId
                                model.teamTokenBalances
                                model.permaFrostedTokens
                        , marketCap =
                            calcMarketCap
                                model.currentFryPriceEth
                                model.currentEthPriceUsd
                                model.circSupply
                        , fullyDiluted =
                            calcFullyDilutedMarketCap
                                model.currentFryPriceEth
                                model.currentEthPriceUsd
                      }
                    , Cmd.none
                    )

        BucketValueEnteredFetched bucketId fetchResult ->
            case fetchResult of
                Err httpErr ->
                    ( model
                    , Cmd.none
                    )

                Ok valueEntered ->
                    ( { model
                        | currentBucketTotalEntered =
                            Just
                                valueEntered
                      }
                    , Cmd.none
                    )

        FetchedTreasuryBalance fetchResult ->
            case fetchResult of
                Err httpErr ->
                    ( model
                    , Cmd.none
                    )

                Ok valueFetched ->
                    ( { model
                        | treasuryBalance =
                            calcTreasuryBalance
                                model.currentDaiPriceEth
                                model.currentEthPriceUsd
                                (Just valueFetched)
                      }
                    , Cmd.none
                    )

        UpdateNow newNow ->
            if calcTimeLeft newNow <= 0 then
                ( model
                , Cmd.none
                )

            else
                ( { model | now = newNow }
                , Cmd.none
                )

        AmountInputChanged newInput ->
            case model.depositWithdrawUXModel of
                Just ( depositOrWithdraw, amountInputUXModel ) ->
                    ( { model
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
                    ( model, Cmd.none )

        UXBack ->
            ( { model
                | depositWithdrawUXModel = Nothing
              }
            , Cmd.none
            )

        DoUnlock ->
            let
                chain =
                    model.wallet
                        |> Wallet.userInfo
                        |> Chain.whenJust
                            (\userInfo ->
                                userInfo.chain
                            )
            in
            ( model
            , doApproveChainCmdFarm chain
                |> attemptTxInitiate model.txSentry model.trackedTxs
            )

        StartDeposit defaultValue ->
            ( { model
                | depositWithdrawUXModel = Just ( Deposit, { amountInput = TokenValue.toFloatString Nothing defaultValue } )
              }
            , Cmd.none
            )

        StartWithdraw defaultValue ->
            ( { model
                | depositWithdrawUXModel = Just ( Withdraw, { amountInput = TokenValue.toFloatString Nothing defaultValue } )
              }
            , Cmd.none
            )

        DoExit ->
            let
                chain =
                    model.wallet
                        |> Wallet.userInfo
                        |> Chain.whenJust
                            (\userInfo ->
                                userInfo.chain
                            )
            in
            ( model
            , doExitChainCmdFarm chain
                |> attemptTxInitiate model.txSentry model.trackedTxs
            )

        DoClaimRewards ->
            let
                chain =
                    model.wallet
                        |> Wallet.userInfo
                        |> Chain.whenJust
                            (\userInfo ->
                                userInfo.chain
                            )
            in
            ( model
            , doClaimRewardsFarm chain
                |> attemptTxInitiate model.txSentry model.trackedTxs
            )

        DoDeposit amount ->
            let
                chain =
                    model.wallet
                        |> Wallet.userInfo
                        |> Chain.whenJust
                            (\userInfo ->
                                userInfo.chain
                            )
            in
            ( model
            , [ doDepositChainCmdFarm chain amount
                    |> attemptTxInitiate model.txSentry model.trackedTxs
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
            let
                chain =
                    model.wallet
                        |> Wallet.userInfo
                        |> Chain.whenJust
                            (\userInfo ->
                                userInfo.chain
                            )
            in
            ( model
            , doWithdrawChainCmdFarm chain amount
                |> attemptTxInitiate model.txSentry model.trackedTxs
            )

        DepositOrWithdrawSigned depositOrWithdraw amount signResult ->
            case signResult of
                Ok txHash ->
                    ( { model
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
                    ( model, Cmd.none )

        RefetchStakingInfoOrApy ->
            let
                chain =
                    model.wallet
                        |> Wallet.userInfo
                        |> Chain.whenJust
                            (\userInfo ->
                                userInfo.chain
                            )
            in
            ( model
            , fetchStakingInfoOrApyCmd chain model.now model.wallet
            )

        StakingInfoFetched fetchResult ->
            case fetchResult of
                Err httpErr ->
                    ( model
                        |> (web3FetchError "staking info" httpErr
                                |> addUserNotice
                           )
                    , Cmd.none
                    )

                Ok ( userStakingInfo, apy ) ->
                    ( { model
                        | userStakingInfo = Just userStakingInfo
                        , apy = Just apy
                      }
                    , Cmd.none
                    )

        ApyFetched fetchResult ->
            case fetchResult of
                Err httpErr ->
                    ( model
                        |> (web3FetchError "apy" httpErr
                                |> addUserNotice
                           )
                    , Cmd.none
                    )

                Ok apy ->
                    ( { model
                        | apy =
                            Just apy
                      }
                    , Cmd.none
                    )

        VerifyJurisdictionClicked ->
            ( { model
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
            ( { model
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
            let
                chain =
                    model.wallet
                        |> Wallet.userInfo
                        |> Chain.whenJust
                            (\userInfo ->
                                userInfo.chain
                            )
            in
            ( model
            , Cmd.batch
                [ refreshPollVotesCmd Nothing
                , fetchFryBalancesCmd chain (model.fryBalances |> AddressDict.keys)
                ]
            )

        PollsFetched pollsFetchedResult ->
            case pollsFetchedResult of
                Err httpErr ->
                    ( model
                        |> (httpFetchError "fetch polls" httpErr |> addUserNotice)
                    , Cmd.none
                    )

                Ok polls ->
                    ( { model
                        | polls = Just polls
                      }
                    , refreshPollVotesCmd Nothing
                    )

        OptionClicked userInfo poll maybePollOptionId ->
            case userInfo of
                Nothing ->
                    ( model
                    , Cmd.none
                    )

                Just val ->
                    ( model
                    , signResponseCmd val poll maybePollOptionId
                    )

        Web3SignResultValue jsonVal ->
            let
                decodedSignResult =
                    Json.Decode.decodeValue signedResponseFromJSDecoder jsonVal
            in
            case decodedSignResult of
                Ok signResult ->
                    ( model
                    , sendSignedResponseCmd signResult
                    )

                Err errStr ->
                    ( model
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
                                            model.maybeValidResponses
                                                |> Dict.get responseId
                                                |> Maybe.map Tuple.second
                                    in
                                    case maybeSignedResponse of
                                        Just signedResponse ->
                                            ( model.validatedResponses
                                                |> insertValidatedResponse ( responseId, signedResponse )
                                            , Just signedResponse.address
                                            , Nothing
                                            )

                                        Nothing ->
                                            ( model.validatedResponses
                                            , Nothing
                                            , Just <| unexpectedError "got a signature verify result from JS, but for response that I don't have!"
                                            )

                                Invalid ->
                                    ( model.validatedResponses
                                    , Nothing
                                    , Nothing
                                    )

                        newBalancesDict =
                            case maybeRespondingAddress of
                                Nothing ->
                                    model.fryBalances

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
                                        model.fryBalances
                                        newDictPortion

                        cmd =
                            let
                                chain =
                                    model.wallet
                                        |> Wallet.userInfo
                                        |> Chain.whenJust
                                            (\userInfo ->
                                                userInfo.chain
                                            )
                            in
                            newBalancesDict
                                |> AddressDict.filter
                                    (\addressString maybeBalance ->
                                        maybeBalance == Nothing
                                    )
                                |> AddressDict.keys
                                |> fetchFryBalancesCmd chain

                        tempModel =
                            case maybeUserNotice of
                                Nothing ->
                                    model

                                Just userNotice ->
                                    model
                                        |> (userNotice
                                                |> addUserNotice
                                           )
                    in
                    ( { tempModel
                        | validatedResponses = newValidatedResponses
                        , maybeValidResponses =
                            model.maybeValidResponses
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
                    ( model
                        |> (unexpectedError "error decoding signature validation from web3js"
                                |> addUserNotice
                           )
                    , Cmd.none
                    )

        ResponseSent pollId sendResult ->
            case sendResult of
                Ok _ ->
                    ( model
                    , refreshPollVotesCmd <| Just pollId
                    )

                Err httpErr ->
                    ( model
                        |> (httpSendError "send response" httpErr
                                |> addUserNotice
                           )
                    , Cmd.none
                    )

        SignedResponsesFetched responsesFetchedResult ->
            case responsesFetchedResult of
                Ok decodedLoggedSignedResponses ->
                    case model.polls of
                        Nothing ->
                            ( model
                                |> (unexpectedError "Responses were fetched, but the polls haven't loaded yet!"
                                        |> addUserNotice
                                   )
                            , Cmd.none
                            )

                        Just polls ->
                            let
                                newMaybeValidResponses =
                                    Dict.union
                                        model.maybeValidResponses
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
                            ( { model
                                | maybeValidResponses = newMaybeValidResponses
                              }
                            , validateSignedResponsesCmd responsesToValidate
                            )

                Err decodeErr ->
                    ( model
                        |> (unexpectedError "error decoding responses from server"
                                |> addUserNotice
                           )
                    , Cmd.none
                    )

        FryBalancesFetched fetchResult ->
            case fetchResult of
                Ok newFryBalances ->
                    ( { model
                        | fryBalances =
                            AddressDict.union
                                (newFryBalances
                                    |> AddressDict.map (always Just)
                                )
                                model.fryBalances
                      }
                    , Cmd.none
                    )

                Err httpErr ->
                    ( model
                        |> (web3FetchError "fetch polls" httpErr
                                |> addUserNotice
                           )
                    , Cmd.none
                    )

        SetMouseoverState newState ->
            ( { model
                | mouseoverState = newState
              }
            , Cmd.none
            )

        DepositAmountChanged amount ->
            let
                chain =
                    model.wallet
                        |> Wallet.userInfo
                        |> Chain.whenJust
                            (\userInfo ->
                                userInfo.chain
                            )
            in
            ( { model
                | depositAmount = amount
              }
            , fetchIssuanceDetail
                chain
                amount
            )

        WithdrawalAmountChanged amount ->
            let
                chain =
                    model.wallet
                        |> Wallet.userInfo
                        |> Chain.whenJust
                            (\userInfo ->
                                userInfo.chain
                            )
            in
            ( { model
                | withDrawalAmount = amount
              }
            , amount
                |> TokenValue.fromString
                |> (fetchDethPositionInfo <| chain)
            )

        DepositClicked amount ->
            ( model
            , (case userInfo model.wallet of
                Nothing ->
                    []

                Just uInfo ->
                    [ doDepositChainCmd
                        uInfo.address
                        amount
                        |> attemptTxInitiate model.txSentry model.trackedTxs
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
            ( model
            , case userInfo model.wallet of
                Nothing ->
                    Cmd.none

                Just uInfo ->
                    doWithdrawChainCmd
                        uInfo.address
                        amount
                        |> attemptTxInitiate model.txSentry model.trackedTxs
            )

        UserEthBalanceFetched fetchResult ->
            case fetchResult of
                Err _ ->
                    ( model
                    , Cmd.none
                    )

                Ok tokenValue ->
                    ( { model
                        | userDerivedEthInfo =
                            (case model.userDerivedEthInfo of
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
                    ( model
                    , Cmd.none
                    )

                Ok tokenValue ->
                    ( { model
                        | userDerivedEthInfo =
                            (case model.userDerivedEthInfo of
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
                    ( model
                    , Cmd.none
                    )

                Ok ( totalCollateral, fee, returnedCollateral ) ->
                    ( { model
                        | userDerivedEthInfo =
                            (case model.userDerivedEthInfo of
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
            ( model
            , Cmd.none
            )

        DepositSigned signResult ->
            case signResult of
                Ok txHash ->
                    let
                        tv =
                            TokenValue.fromString model.depositAmount

                        amount =
                            case tv of
                                Nothing ->
                                    TokenValue.zero

                                Just val ->
                                    val
                    in
                    ( { model
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
                    ( model
                    , Cmd.none
                    )

        WithdrawSigned signResult ->
            case signResult of
                Ok txHash ->
                    let
                        tv =
                            TokenValue.fromString model.depositAmount

                        amount =
                            case tv of
                                Nothing ->
                                    TokenValue.zero

                                Just val ->
                                    val
                    in
                    ( { model
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
                    ( model
                    , Cmd.none
                    )

        FetchUserEthBalance ->
            let
                chain =
                    model.wallet
                        |> Wallet.userInfo
                        |> Chain.whenJust
                            (\userInfo ->
                                userInfo.chain
                            )
            in
            ( model
            , model.wallet
                |> fetchEthBalance chain
            )

        FetchUserDerivedEthBalance ->
            let
                chain =
                    model.wallet
                        |> Wallet.userInfo
                        |> Chain.whenJust
                            (\userInfo ->
                                userInfo.chain
                            )
            in
            ( model
            , model.wallet
                |> (chain
                        |> fetchDerivedEthBalance
                   )
            )

        DerivedEthIssuanceDetailFetched fetchResult ->
            case fetchResult of
                Err _ ->
                    ( model
                    , Cmd.none
                    )

                Ok ( actualCollateralAdded, depositFee, tokensIssued ) ->
                    ( { model
                        | userDerivedEthInfo =
                            (case model.userDerivedEthInfo of
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
            model
                |> gotoRoute route
                |> Tuple.mapSecond
                    (\cmd ->
                        Cmd.batch
                            [ cmd
                            , Browser.Navigation.pushUrl
                                model.navKey
                                (Routing.routeToString model.basePath route)
                            ]
                    )

        ConnectToWeb3 ->
            let
                gtagCmd =
                    GTagData
                        "wallet connect initiated"
                        Nothing
                        Nothing
                        Nothing
                        |> gTagOut
            in
            ( { model
                | wallet = Connecting
              }
            , [ Ports.connectToWeb3 ()
              , gtagCmd
              ]
                |> Cmd.batch
            )

        ShowOrHideAddress phaceId ->
            ( { model
                | showAddressId =
                    if model.showAddressId == Just phaceId then
                        Nothing

                    else
                        Just phaceId
              }
            , Cmd.none
            )

        AddUserNotice userNotice ->
            ( model
                |> addUserNotice userNotice
            , Cmd.none
            )

        BSCImport ->
            let
                address =
                    case userInfo model.wallet of
                        Nothing ->
                            "not connected"

                        Just userInfo ->
                            userInfo.address
                                |> Eth.Utils.addressToString

                gtagCmd =
                    GTagData
                        "xdai import clicked"
                        Nothing
                        (address
                            |> Just
                        )
                        Nothing
                        |> gTagOut
            in
            ( { model | chainSwitchInProgress = True }
            , [ Ports.bscImport ()
              , gtagCmd
              ]
                |> Cmd.batch
            )

        WalletResponse res ->
            res
                |> unpack
                    (\err ->
                        case err of
                            WalletInProgress ->
                                ( { model
                                    | userNotices = UN.unexpectedError "Please complete the wallet connection process." :: model.userNotices
                                  }
                                , Cmd.none
                                )

                            WalletCancel ->
                                ( { model
                                    | userNotices = UN.unexpectedError "The wallet connection has been cancelled." :: model.userNotices
                                    , wallet = NetworkReady
                                    , chainSwitchInProgress = False
                                  }
                                , Cmd.none
                                )

                            NetworkNotSupported ->
                                ( { model
                                    | userNotices = UN.unexpectedError "This network is not supported by SmokeSignal." :: model.userNotices
                                    , wallet = NetworkReady
                                    , chainSwitchInProgress = False
                                  }
                                , Cmd.none
                                )

                            WalletError e ->
                                ( { model
                                    | wallet =
                                        Types.NetworkReady
                                    , chainSwitchInProgress = False
                                  }
                                , Ports.log e
                                )
                    )
                    (\info ->
                        let
                            ( gtagHistory, walletConnectedGtagCmd ) =
                                GTag.gTagOutOnlyOnceForEvent model.gtagHistory <|
                                    GTagData
                                        "wallet connected"
                                        Nothing
                                        (Just <| Eth.Utils.addressToString info.address)
                                        Nothing
                        in
                        ( { model
                            | wallet = Active info
                            , chainSwitchInProgress = False
                            , gtagHistory = gtagHistory
                          }
                        , walletConnectedGtagCmd
                        )
                    )

        ChainSwitchResponse res ->
            res
                |> unpack
                    (\err ->
                        case err of
                            Types.UserRejected ->
                                ( { model
                                    | chainSwitchInProgress = False
                                  }
                                , Cmd.none
                                )

                            Types.OtherErr e ->
                                ( { model
                                    | chainSwitchInProgress = False
                                  }
                                , Ports.log e
                                )
                    )
                    (\() ->
                        ( -- Wait for WalletResponse to update model.chainSwitchInProgress
                          model
                        , Cmd.none
                        )
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
