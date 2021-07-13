module Update exposing (..)

import AddressDict exposing (AddressDict)
import Array exposing (Array)
import BigInt
import Browser
import Browser.Navigation
import Chain
import Config
import Contracts.DEthWrapper as Deth
import Contracts.Generated.ERC20 as ERC20
import Contracts.Generated.StakingRewards as StakingRewardsContract
import Dict
import Dict.Extra
import ElementHelpers as EH exposing (DisplayProfile(..))
import Eth
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Sentry.Tx as TxSentry exposing (TxSentry)
import Eth.Utils
import GTag exposing (GTagData, gTagOut)
import Helpers.Eth as EthHelpers
import Helpers.Tuple exposing (tuple3MapSecond, tuple3Second, tuple3ToList)
import Json.Decode
import List.Extra
import Maybe.Extra exposing (unwrap)
import Misc exposing (..)
import Ports
import Result.Extra exposing (unpack)
import Routing
import Time
import TokenValue
import Types exposing (..)
import Url
import UserNotice as UN exposing (UserNotice, cantConnectNoWeb3, httpFetchError, httpSendError, routeNotFound, signingError, unexpectedError, walletError, web3FetchError)
import UserTx exposing (Initiator, SignedTxStatus(..), TxInfo(..))
import Wallet


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ensureUserInfo fn =
            model.wallet
                |> Wallet.userInfo
                |> unwrap ( model, Ports.log "Missing wallet" ) fn
    in
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
            ( { model
                | now = i
                , currentTime = Time.posixToMillis i
                , currentBucketId = getCurrentBucketId <| Time.posixToMillis i
              }
            , refreshCmds model.wallet model.withdrawalAmountInput model.currentBucketId
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
                                |> Wallet.getChainDefaultEth

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
                    , case model.wallet |> Wallet.userInfo |> Maybe.map .address of
                        Just userAddress ->
                            [ fetchDerivedEthBalance userAddress
                            , fetchEthBalance userAddress
                      ]
                        |> Cmd.batch

                        Nothing ->
                            Cmd.none
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

        FetchedTreasuryBalance treasuryIndex fetchResult ->
            case fetchResult of
                Err httpErr ->
                    ( model
                    , Cmd.none
                    )

                Ok valueFetched ->
                    ( { model
                        | composedTreasuryBalance =
                            model.composedTreasuryBalance
                                |> List.Extra.setAt treasuryIndex
                                    (Just valueFetched)
                      }
                    , Cmd.none
                    )

        UpdateNow newNow ->
            if calcTimeLeft newNow model.farmingPeriodEnds <= 0 then
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
            ensureUserInfo
                (\userInfo ->
                    let
                        txParams =
                            ERC20.approve
                                Config.stakingLiquidityContractAddress
                                Config.stakingContractAddress
                                (TokenValue.maxTokenValue |> TokenValue.getEvmValue)
                                |> (\call ->
                                        { call | from = Just userInfo.address }
                                   )
                                |> Eth.toSend
                                |> Eth.encodeSend
                    in
                    ( model
                    , Ports.txSend txParams
                    )
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
            ensureUserInfo
                (\userInfo ->
                    let
                        txParams =
                            StakingRewardsContract.exit
                                Config.stakingContractAddress
                                |> (\call ->
                                        { call | from = Just userInfo.address }
                                   )
                                |> Eth.toSend
                                |> Eth.encodeSend
                    in
                    ( model
                    , Ports.txSend txParams
                    )
                )

        DoClaimRewards ->
            ensureUserInfo
                (\userInfo ->
                    let
                        txParams =
                            StakingRewardsContract.getReward
                                Config.stakingContractAddress
                                |> (\call ->
                                        { call | from = Just userInfo.address }
                                   )
                                |> Eth.toSend
                                |> Eth.encodeSend
                    in
                    ( model
                    , Ports.txSend txParams
                    )
                )

        DoDeposit amount ->
            ensureUserInfo
                (\userInfo ->
                    let
                        txParams =
                            StakingRewardsContract.stake
                                Config.stakingContractAddress
                                (TokenValue.getEvmValue amount)
                                |> (\call ->
                                        { call | from = Just userInfo.address }
                                   )
                                |> Eth.toSend
                                |> Eth.encodeSend
                    in
                    ( model
                    , [ Ports.txSend txParams
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
                )

        DoWithdraw amount ->
            ensureUserInfo
                (\userInfo ->
                    let
                        txParams =
                            StakingRewardsContract.withdraw
                                Config.stakingContractAddress
                                (TokenValue.getEvmValue amount)
                                |> (\call ->
                                        { call | from = Just userInfo.address }
                                   )
                                |> Eth.toSend
                                |> Eth.encodeSend
                    in
                    ( model
                    , Ports.txSend txParams
                    )
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
            ensureUserInfo
                (\userInfo ->
                    ( model
                    , [ fetchStakingInfoOrApyCmd model.wallet
                      , fetchApyCmd
                      ]
                        |> Cmd.batch
                    )
                )

        StakingInfoFetched fetchResult ->
            let
                chain =
                    model.wallet
                        |> Wallet.getChainDefaultEth
            in
            case fetchResult of
                Err httpErr ->
                    ( --     if chain == Eth || chain == BSC then
                      --     model
                      --         |> (web3FetchError "staking info" httpErr
                      --                 |> addUserNotice
                      --            )
                      --   else
                      model
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
            let
                chain =
                    model.wallet
                        |> Wallet.getChainDefaultEth
            in
            case fetchResult of
                Err httpErr ->
                    ( --     if chain == Eth || chain == BSC then
                      --     model
                      --         |> (web3FetchError "apy" httpErr
                      --                 |> addUserNotice
                      --            )
                      --   else
                      model
                    , Cmd.none
                    )

                Ok apy ->
                    ( { model
                        | apy =
                            Just apy
                      }
                    , Cmd.none
                    )

        RefreshAll ->
            ensureUserInfo
                (\userInfo ->
                    ( model
                    , Cmd.batch
                        [ refreshPollVotesCmd model Nothing
                        , fetchFryBalancesCmd (model.fryBalances |> AddressDict.keys)
                        ]
                    )
                )

        PollsFetched pollsFetchedResult ->
            case pollsFetchedResult of
                Err httpErr ->
                    ( model
                      -- |> (httpFetchError "fetch polls" httpErr |> addUserNotice)
                    , Cmd.none
                    )

                Ok polls ->
                    ( { model
                        | polls = Just polls
                      }
                    , refreshPollVotesCmd model Nothing
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
                                        |> Wallet.getChainDefaultEth
                            in
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
                    , refreshPollVotesCmd model <| Just pollId
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
                    let
                        chain =
                            model.wallet
                                |> Wallet.getChainDefaultEth
                    in
                    ( case chain of
                        Eth ->
                            model
                                |> (web3FetchError "fetch polls" httpErr
                                        |> addUserNotice
                                   )

                        _ ->
                            model
                    , Cmd.none
                    )

        SetMouseoverState newState ->
            ( { model
                | mouseoverState = newState
              }
            , Cmd.none
            )

        DepositAmountChanged amount ->
            ensureUserInfo
                (\userInfo ->
                    ( { model
                        | depositAmount = amount
                      }
                    , fetchIssuanceDetail
                        userInfo.chain
                        amount
                    )
                )

        WithdrawalAmountChanged amountInput ->
            ensureUserInfo
                (\userInfo ->
                    ( { model
                        | withdrawalAmountInput = amountInput
                      }
                    , Maybe.map fetchDethPositionInfo (TokenValue.fromString amountInput)
                        |> Maybe.withDefault Cmd.none
                    )
                )

        DepositClicked amount ->
            ( model
            , (case userInfo model.wallet of
                Nothing ->
                    []

                Just uInfo ->
                    [ let
                        txParams =
                            Deth.deposit
                                Config.derivedEthContractAddress
                                uInfo.address
                                amount
                                |> EthHelpers.addFrom uInfo.address
                                |> Eth.encodeSend
                      in
                      Ports.txSend txParams
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
                    let
                        txParams =
                            Deth.redeem
                                Config.derivedEthContractAddress
                                uInfo.address
                                amount
                                |> EthHelpers.addFrom uInfo.address
                                |> Eth.encodeSend
                    in
                    Ports.txSend txParams
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
                                    { derivedEthInfoInit
                                        | ethBalance = tokenValue
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
                                    { derivedEthInfoInit
                                        | dEthBalance = tokenValue
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

                Ok data ->
                    let
                        redeemFee =
                            { protocolFee = TokenValue.tokenValue data.protocolFee
                            , automationFee = TokenValue.tokenValue data.automationFee
                            }
                    in
                    ( { model
                        | userDerivedEthInfo =
                            (case model.userDerivedEthInfo of
                                Nothing ->
                                    { derivedEthInfoInit
                                        | totalCollateralRedeemed = TokenValue.tokenValue data.collateralRedeemed
                                        , redeemFee = redeemFee
                                        , collateralReturned = TokenValue.tokenValue data.collateralReturned
                                    }

                                Just oldUserDerivedEthInfoModel ->
                                    { oldUserDerivedEthInfoModel
                                        | totalCollateralRedeemed = TokenValue.tokenValue data.collateralRedeemed
                                        , redeemFee = redeemFee
                                        , collateralReturned = TokenValue.tokenValue data.collateralReturned
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
                        | withdrawalAmountInput = ""
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
            ( model
            , Maybe.map fetchEthBalance
                (model.wallet |> Wallet.userInfo |> Maybe.map .address)
                |> Maybe.withDefault Cmd.none
            )

        FetchUserDerivedEthBalance ->
            ( model
            , Maybe.map fetchDerivedEthBalance
                (model.wallet |> Wallet.userInfo |> Maybe.map .address)
                |> Maybe.withDefault Cmd.none
            )

        DerivedEthIssuanceDetailFetched fetchResult ->
            case fetchResult of
                Err _ ->
                    ( model
                    , Cmd.none
                    )

                Ok data ->
                    let
                        depositFee =
                            { protocolFee = TokenValue.tokenValue data.protocolFee
                            , automationFee = TokenValue.tokenValue data.automationFee
                            }
                    in
                    ( { model
                        | userDerivedEthInfo =
                            (case model.userDerivedEthInfo of
                                Nothing ->
                                    { derivedEthInfoInit
                                        | actualCollateralAdded = TokenValue.tokenValue data.actualCollateralAdded
                                        , depositFee = depositFee
                                        , tokensIssued = TokenValue.tokenValue data.tokensIssued
                                    }

                                Just oldUserDerivedEthInfoModel ->
                                    { oldUserDerivedEthInfoModel
                                        | actualCollateralAdded = TokenValue.tokenValue data.actualCollateralAdded
                                        , depositFee = depositFee
                                        , tokensIssued = TokenValue.tokenValue data.tokensIssued
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

        -- BSCImport ->
        --     ensureUserInfo
        --         (\userInfo ->
        --             let
        --                 address =
        --                     userInfo.address
        --                         |> Eth.Utils.addressToString
        --                 gtagCmd =
        --                     GTagData
        --                         "bsc import clicked"
        --                         Nothing
        --                         (address
        --                             |> Just
        --                         )
        --                         Nothing
        --                         |> gTagOut
        --             in
        --             ( { model
        --                 | chainSwitchInProgress = True
        --                 , userStakingInfo = Nothing
        --               }
        --             , [ Ports.bscImport ()
        --               , gtagCmd
        --               ]
        --                 |> Cmd.batch
        --             )
        --         )
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
                                    | userNotices = UN.unexpectedError "This network is not supported." :: model.userNotices
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
                        , [ walletConnectedGtagCmd
                          , fetchStakingInfoOrApyCmd model.wallet
                          ]
                            |> Cmd.batch
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
                          { model
                            | userStakingInfo = Nothing
                          }
                        , Cmd.none
                        )
                    )

        TxSendResponse res ->
            ensureUserInfo
                (\userInfo ->
                    res
                        |> unpack
                            (\err ->
                                case err of
                                    Types.UserRejected ->
                                        let
                                            gtagCmd =
                                                GTagData
                                                    "tx rejected"
                                                    ("tx rejected" |> Just)
                                                    Nothing
                                                    Nothing
                                                    |> gTagOut
                                        in
                                        ( model
                                        , gtagCmd
                                        )

                                    Types.OtherErr e ->
                                        let
                                            gtagCmd =
                                                GTagData
                                                    "tx error"
                                                    ("tx error" |> Just)
                                                    Nothing
                                                    Nothing
                                                    |> gTagOut
                                        in
                                        ( model
                                        , [ Ports.log e
                                          , gtagCmd
                                          ]
                                            |> Cmd.batch
                                        )
                            )
                            (\txHash ->
                                ( { model
                                    | userNotices =
                                        model.userNotices
                                            |> List.append [ UN.notify "Your transaction is mining." ]

                                    -- , trackedTxs =
                                    --     model.trackedTxs
                                    --         |> Dict.insert (Eth.Utils.txHashToString txHash)
                                    --             { txHash = txHash
                                    --             , txInfo = Send txHash
                                    --             , status = Mining
                                    --             , chain = userInfo.chain
                                    --             }
                                  }
                                , Cmd.none
                                )
                            )
                )

        FetchFarmingPeriodEnd ->
            let
                chain =
                    model.wallet
                        |> Wallet.getChainDefaultEth
            in
            ( model
            , fetchFarmEndTime
            )

        FarmingPeriodEndFetched fetchResult ->
            case fetchResult of
                Err _ ->
                    ( model
                    , Cmd.none
                    )

                Ok periodFinish ->
                    ( { model
                        | farmingPeriodEnds =
                            periodFinish
                                |> BigInt.toString
                                |> String.toInt
                                |> Maybe.withDefault 0
                      }
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
