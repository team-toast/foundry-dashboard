port module State exposing (init, subscriptions, update)

import Array exposing (Array)
import Browser
import Browser.Events
import Browser.Navigation
import Common.Msg exposing (..)
import Common.Types exposing (..)
import Common.View
import Config
import Contracts.Dai as Dai
import Dict exposing (Dict)
import Eth
import Eth.Decode
import Eth.Net
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Sentry.Tx as TxSentry
import Eth.Sentry.Wallet as WalletSentry
import Eth.Types exposing (Address, TxHash)
import Eth.Utils
import Farm.State as Farm
import Helpers.Element as EH exposing (DisplayProfile(..))
import Helpers.Tuple as TupleHelpers
import Home.State as Home
import Json.Decode
import Json.Encode
import List.Extra
import Maybe.Extra
import MaybeDebugLog exposing (maybeDebugLog)
import Random
import Routing exposing (Route)
import Sentiment.State as Sentiment
import Stats.State as Stats
import Task
import Time
import TokenValue exposing (TokenValue)
import Types exposing (..)
import Url exposing (Url)
import UserNotice as UN exposing (UserNotice)
import UserTx exposing (TxInfo)
import Wallet


init :
    Flags
    -> Url
    -> Browser.Navigation.Key
    -> ( Model, Cmd Msg )
init flags url key =
    let
        route =
            Routing.urlToRoute url

        ( wallet, walletNotices ) =
            if flags.networkId == 0 then
                ( Wallet.NoneDetected
                , [ UN.noWeb3Provider ]
                )

            else
                ( Wallet.OnlyNetwork <| Eth.Net.toNetworkId flags.networkId
                , []
                )

        txSentry =
            TxSentry.init
                ( txOut, txIn )
                TxSentryMsg
                Config.httpProviderUrl

        ( eventSentry, eventSentryCmd ) =
            EventSentry.init EventSentryMsg Config.httpProviderUrl

        now =
            Time.millisToPosix flags.nowInMillis

        homeUpdateResult =
            Home.init
    in
    { navKey = key
    , basePath = flags.basePath
    , route = route
    , wallet = wallet
    , now = now
    , dProfile = EH.screenWidthToDisplayProfile flags.width
    , txSentry = txSentry
    , eventSentry = eventSentry
    , submodel = BlankInitialSubmodel
    , showAddressId = Nothing
    , userNotices = walletNotices
    , trackedTxs = []
    , trackedTxsExpanded = False
    , nonRepeatingGTagsSent = []
    , cookieConsentGranted = flags.cookieConsent
    }
        |> gotoRoute route
        |> Tuple.mapSecond
            (\routeCmd ->
                Cmd.batch
                    [ eventSentryCmd
                    , routeCmd
                    ]
            )


update :
    Msg
    -> Model
    -> ( Model, Cmd Msg )
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
              -- , Wallet.userInfo prevModel.wallet
              --     |> Maybe.map
              --         (\userInfo ->
              --             fetchDaiBalanceAndAllowanceCmd userInfo.address
              --         )
              --     |> Maybe.withDefault Cmd.none
            )

        WalletStatus walletSentryResult ->
            case walletSentryResult of
                Ok walletSentry ->
                    let
                        ( newWallet, notifySubmodel ) =
                            case walletSentry.account of
                                Just newAddress ->
                                    if (prevModel.wallet |> Wallet.userInfo |> Maybe.map .address) == Just newAddress then
                                        ( prevModel.wallet, False )

                                    else
                                        ( Wallet.Active <|
                                            UserInfo
                                                walletSentry.networkId
                                                newAddress
                                          -- Nothing
                                          -- Checking
                                        , True
                                        )

                                Nothing ->
                                    ( Wallet.OnlyNetwork walletSentry.networkId
                                    , prevModel.wallet /= Wallet.OnlyNetwork walletSentry.networkId
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
                    ( prevModel |> addUserNotice (UN.walletError errStr)
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

        -- BalanceFetched address fetchResult ->
        --     let
        --         maybeCurrentAddress =
        --             Wallet.userInfo prevModel.wallet
        --                 |> Maybe.map .address
        --     in
        --     if maybeCurrentAddress /= Just address then
        --         ( prevModel, Cmd.none )
        --     else
        --         case fetchResult of
        --             Ok balance ->
        --                 let
        --                     newWallet =
        --                         prevModel.wallet |> Wallet.withFetchedBalance balance
        --                 in
        --                 ( { prevModel
        --                     | wallet = newWallet
        --                   }
        --                 , Cmd.none
        --                 )
        --             Err httpErr ->
        --                 ( prevModel
        --                     |> addUserNotice (UN.web3FetchError "DAI balance" httpErr)
        --                 , Cmd.none
        --                 )
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

        HomeMsg homeMsg ->
            case prevModel.submodel of
                Home homeModel ->
                    let
                        updateResult =
                            homeModel
                                |> Home.update homeMsg
                    in
                    ( { prevModel
                        | submodel =
                            Home updateResult.newModel
                      }
                    , Cmd.map HomeMsg updateResult.cmd
                    )
                        |> withMsgUps updateResult.msgUps

                _ ->
                    ( prevModel, Cmd.none )

        SentimentMsg sentimentMsg ->
            case prevModel.submodel of
                Sentiment sentimentModel ->
                    let
                        updateResult =
                            sentimentModel
                                |> Sentiment.update sentimentMsg
                    in
                    ( { prevModel
                        | submodel =
                            Sentiment updateResult.newModel
                      }
                    , Cmd.map SentimentMsg updateResult.cmd
                    )
                        |> withMsgUps updateResult.msgUps

                _ ->
                    ( prevModel, Cmd.none )

        StatsMsg statsMsg ->
            case prevModel.submodel of
                Stats statsModel ->
                    let
                        updateResult =
                            statsModel
                                |> Stats.update statsMsg
                    in
                    ( { prevModel
                        | submodel =
                            Stats updateResult.newModel
                      }
                    , Cmd.map StatsMsg updateResult.cmd
                    )
                        |> withMsgUps updateResult.msgUps

                _ ->
                    ( prevModel, Cmd.none )

        FarmMsg farmMsg ->
            case prevModel.submodel of
                Farm farmModel ->
                    let
                        updateResult =
                            farmModel
                                |> Farm.update farmMsg

                        ( newTxSentry, sentryCmd, newTrackedTxs ) =
                            initiateUserTxs
                                prevModel.txSentry
                                prevModel.trackedTxs
                                (UserTx.mapInitiatorList FarmMsg updateResult.userTxs)
                    in
                    ( { prevModel
                        | txSentry = newTxSentry
                        , submodel = Farm updateResult.newModel
                        , trackedTxs = newTrackedTxs
                      }
                    , Cmd.batch
                        [ Cmd.map FarmMsg updateResult.cmd
                        , sentryCmd
                        ]
                    )
                        |> withMsgUps
                            updateResult.msgUps

                _ ->
                    ( prevModel, Cmd.none )

        CookieConsentGranted ->
            ( { prevModel
                | cookieConsentGranted = True
              }
            , Cmd.batch
                [ consentToCookies ()
                , gTagOut <|
                    encodeGTag <|
                        GTagData
                            "accept cookies"
                            ""
                            ""
                            0
                ]
            )

        MsgUp msgUp ->
            prevModel |> handleMsgUp msgUp

        ClickHappened ->
            ( { prevModel
                | showAddressId = Nothing
              }
            , Cmd.none
            )

        Types.NoOp ->
            ( prevModel, Cmd.none )


handleMsgUp :
    MsgUp
    -> Model
    -> ( Model, Cmd Msg )
handleMsgUp msgUp prevModel =
    case msgUp of
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
                Wallet.NoneDetected ->
                    ( prevModel |> addUserNotice UN.cantConnectNoWeb3
                    , Cmd.none
                    )

                _ ->
                    ( prevModel
                    , connectToWeb3 ()
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
            , gTagOut (encodeGTag gtag)
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
                , gTagOut (encodeGTag gtag)
                )

        Common.Msg.NoOp ->
            ( prevModel, Cmd.none )


withMsgUp :
    MsgUp
    -> ( Model, Cmd Msg )
    -> ( Model, Cmd Msg )
withMsgUp msgUp ( prevModel, prevCmd ) =
    handleMsgUp msgUp prevModel
        |> Tuple.mapSecond
            (\newCmd ->
                Cmd.batch [ prevCmd, newCmd ]
            )


handleMsgUps :
    List MsgUp
    -> Model
    -> ( Model, Cmd Msg )
handleMsgUps msgUps prevModel =
    List.foldl
        withMsgUp
        ( prevModel, Cmd.none )
        msgUps


withMsgUps :
    List MsgUp
    -> ( Model, Cmd Msg )
    -> ( Model, Cmd Msg )
withMsgUps msgUps ( prevModel, prevCmd ) =
    handleMsgUps msgUps prevModel
        |> Tuple.mapSecond
            (\newCmd ->
                Cmd.batch [ prevCmd, newCmd ]
            )


initiateUserTxs :
    TxSentry.TxSentry Msg
    -> UserTx.Tracker Msg
    -> List (UserTx.Initiator Msg)
    -> ( TxSentry.TxSentry Msg, Cmd Msg, UserTx.Tracker Msg )
initiateUserTxs txSentry prevTrackedTxs txInitiators =
    List.foldl
        (\initiator ( accTxSentry, accCmd, accTrackedTxs ) ->
            initiateUserTx accTxSentry accTrackedTxs initiator
                |> TupleHelpers.tuple3MapSecond
                    (\newCmd ->
                        Cmd.batch
                            [ accCmd
                            , newCmd
                            ]
                    )
        )
        ( txSentry, Cmd.none, prevTrackedTxs )
        txInitiators


initiateUserTx :
    TxSentry.TxSentry Msg
    -> UserTx.Tracker Msg
    -> UserTx.Initiator Msg
    -> ( TxSentry.TxSentry Msg, Cmd Msg, UserTx.Tracker Msg )
initiateUserTx txSentry prevTrackedTxs txInitiator =
    let
        ( trackedTxId, newTrackedTxs ) =
            prevTrackedTxs
                |> addTrackedTx txInitiator.txInfo txInitiator.notifiers

        ( newTxSentry, txSentryCmd ) =
            UserTx.execute
                txSentry
                { txInitiator
                    | notifiers = trackingNotifiers trackedTxId
                }
    in
    ( newTxSentry
    , txSentryCmd
    , newTrackedTxs
    )


trackingNotifiers :
    Int
    -> UserTx.Notifiers Msg
trackingNotifiers trackedTxId =
    { onSign = Just <| TxSigned trackedTxId

    -- , onBroadcast = Nothing
    -- , onBroadcast = Just <| TxBroadcast trackedTxId
    , onMine = Just <| TxMined trackedTxId
    }


addTrackedTx :
    TxInfo
    -> UserTx.Notifiers Msg
    -> UserTx.Tracker Msg
    -> ( Int, UserTx.Tracker Msg )
addTrackedTx userTx notifiers tracker =
    let
        newTrackedTx =
            UserTx.TrackedTx
                notifiers
                userTx
                UserTx.Signing
    in
    ( List.length tracker
    , List.append
        tracker
        [ newTrackedTx ]
    )


updateFromPageRoute :
    Route
    -> Model
    -> ( Model, Cmd Msg )
updateFromPageRoute route model =
    if model.route == route then
        ( model
        , Cmd.none
        )

    else
        gotoRoute route model


gotoRoute :
    Route
    -> Model
    -> ( Model, Cmd Msg )
gotoRoute route prevModel =
    case route of
        Routing.Home ->
            let
                updateResult =
                    Home.init
            in
            ( { prevModel
                | route = route
                , submodel = Home updateResult.newModel
              }
            , Cmd.map HomeMsg updateResult.cmd
            )
                |> withMsgUps updateResult.msgUps

        Routing.Sentiment ->
            let
                ( sentimentModel, sentimentCmd ) =
                    Sentiment.init
            in
            ( { prevModel
                | route = route
                , submodel = Sentiment sentimentModel
              }
            , Cmd.map SentimentMsg sentimentCmd
            )

        Routing.Stats ->
            let
                ( statsModel, statsCmd ) =
                    Stats.init <| Time.posixToMillis prevModel.now
            in
            ( { prevModel
                | route = route
                , submodel = Stats statsModel
              }
            , Cmd.map StatsMsg statsCmd
            )

        Routing.Farm ->
            let
                ( farmModel, farmCmd ) =
                    Farm.init prevModel.wallet prevModel.now
            in
            ( { prevModel
                | route = route
                , submodel = Farm farmModel
              }
            , Cmd.map FarmMsg farmCmd
            )

        Routing.NotFound err ->
            ( { prevModel
                | route = route
              }
                |> addUserNotice UN.routeNotFound
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


runMsgDown : MsgDown -> Model -> ( Model, Cmd Msg )
runMsgDown msg prevModel =
    case prevModel.submodel of
        BlankInitialSubmodel ->
            ( prevModel, Cmd.none )

        Home homeModel ->
            let
                updateResult =
                    homeModel |> Home.runMsgDown msg

                newSubmodel =
                    Home updateResult.newModel
            in
            ( { prevModel
                | submodel = newSubmodel
              }
            , Cmd.map HomeMsg updateResult.cmd
            )
                |> withMsgUps updateResult.msgUps

        Sentiment sentimentModel ->
            let
                updateResult =
                    sentimentModel |> Sentiment.runMsgDown msg

                newSubmodel =
                    Sentiment updateResult.newModel
            in
            ( { prevModel
                | submodel = newSubmodel
              }
            , Cmd.map SentimentMsg updateResult.cmd
            )
                |> withMsgUps updateResult.msgUps

        Stats statsModel ->
            let
                updateResult =
                    statsModel |> Stats.runMsgDown msg

                newSubmodel =
                    Stats updateResult.newModel
            in
            ( { prevModel
                | submodel = newSubmodel
              }
            , Cmd.map StatsMsg updateResult.cmd
            )
                |> withMsgUps updateResult.msgUps

        Farm farmModel ->
            let
                updateResult =
                    farmModel |> Farm.runMsgDown msg

                newSubmodel =
                    Farm updateResult.newModel
            in
            ( { prevModel
                | submodel = newSubmodel
              }
            , Cmd.map FarmMsg updateResult.cmd
            )
                |> withMsgUps updateResult.msgUps


encodeGTag : GTagData -> Json.Decode.Value
encodeGTag gtag =
    Json.Encode.object
        [ ( "event", Json.Encode.string gtag.event )
        , ( "category", Json.Encode.string gtag.category )
        , ( "label", Json.Encode.string gtag.label )
        , ( "value", Json.Encode.int gtag.value )
        ]


submodelSubscriptions : Submodel -> Sub Msg
submodelSubscriptions submodel =
    case submodel of
        BlankInitialSubmodel ->
            Sub.none

        Home homeModel ->
            Sub.map
                HomeMsg
                (Home.subscriptions homeModel)

        Sentiment sentimentModel ->
            Sub.map
                SentimentMsg
                (Sentiment.subscriptions sentimentModel)

        Stats statsModel ->
            Sub.map
                StatsMsg
                (Stats.subscriptions statsModel)

        Farm farmModel ->
            Sub.map
                FarmMsg
                (Farm.subscriptions farmModel)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 200 Tick
        , Time.every 2500 (always EveryFewSeconds)
        , walletSentryPort
            (WalletSentry.decodeToMsg
                (WalletStatus << Err)
                (WalletStatus << Ok)
            )
        , TxSentry.listen model.txSentry
        , Browser.Events.onResize Resize
        , submodelSubscriptions model.submodel
        ]


port walletSentryPort : (Json.Decode.Value -> msg) -> Sub msg


port connectToWeb3 : () -> Cmd msg


port txOut : Json.Decode.Value -> Cmd msg


port txIn : (Json.Decode.Value -> msg) -> Sub msg


port gTagOut : Json.Decode.Value -> Cmd msg


port consentToCookies : () -> Cmd msg
