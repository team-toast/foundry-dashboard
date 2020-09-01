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
import Helpers.Element as EH exposing (DisplayProfile(..))
import Home.State as Home
import Json.Decode
import Json.Encode
import List.Extra
import Maybe.Extra
import MaybeDebugLog exposing (maybeDebugLog)
import Random
import Routing exposing (Route)
import Sentiment.State as Sentiment
import Task
import Time
import TokenValue exposing (TokenValue)
import Types exposing (..)
import Url exposing (Url)
import UserNotice as UN exposing (UserNotice)
import Wallet


init : Flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
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

        ( homeModel, homeModelCmd ) =
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
    , submodel = Home homeModel
    , showAddressId = Nothing
    , userNotices = walletNotices
    }
        |> gotoRoute route
        |> Tuple.mapSecond
            (\routeCmd ->
                Cmd.batch
                    [ eventSentryCmd
                    , routeCmd
                    , Cmd.map HomeMsg homeModelCmd
                    ]
            )


initDemoPhaceSrc : String
initDemoPhaceSrc =
    "2222222222222222222222222228083888c8f222"


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
                        newWallet =
                            case walletSentry.account of
                                Just newAddress ->
                                    if (prevModel.wallet |> Wallet.userInfo |> Maybe.map .address) == Just newAddress then
                                        prevModel.wallet

                                    else
                                        Wallet.Active <|
                                            UserInfo
                                                walletSentry.networkId
                                                newAddress
                                                Nothing
                                                Checking

                                Nothing ->
                                    Wallet.OnlyNetwork walletSentry.networkId
                    in
                    ( { prevModel
                        | wallet = newWallet
                      }
                    , Cmd.none
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

        MsgUp msgUp ->
            prevModel |> handleMsgUp msgUp

        ClickHappened ->
            ( { prevModel
                | showAddressId = Nothing
              }
            , Cmd.none
            )


handleMsgUp : MsgUp -> Model -> ( Model, Cmd Msg )
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

        NoOp ->
            ( prevModel, Cmd.none )


withMsgUp : MsgUp -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withMsgUp msgUp ( prevModel, prevCmd ) =
    handleMsgUp msgUp prevModel
        |> Tuple.mapSecond
            (\newCmd ->
                Cmd.batch [ prevCmd, newCmd ]
            )


handleMsgUps : List MsgUp -> Model -> ( Model, Cmd Msg )
handleMsgUps msgUps prevModel =
    List.foldl
        withMsgUp
        ( prevModel, Cmd.none )
        msgUps


withMsgUps : List MsgUp -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withMsgUps msgUps ( prevModel, prevCmd ) =
    handleMsgUps msgUps prevModel
        |> Tuple.mapSecond
            (\newCmd ->
                Cmd.batch [ prevCmd, newCmd ]
            )


updateFromPageRoute : Route -> Model -> ( Model, Cmd Msg )
updateFromPageRoute route model =
    if model.route == route then
        ( model
        , Cmd.none
        )

    else
        gotoRoute route model


gotoRoute : Route -> Model -> ( Model, Cmd Msg )
gotoRoute route prevModel =
    case route of
        Routing.Home ->
            let
                ( homeModel, homeCmd ) =
                    Home.init
            in
            ( { prevModel
                | route = route
                , submodel = Home homeModel
              }
            , Cmd.map HomeMsg homeCmd
            )

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


submodelSubscriptions : Submodel -> Sub Msg
submodelSubscriptions submodel =
    case submodel of
        Home homeModel ->
            Sub.map
                HomeMsg
                (Home.subscriptions homeModel)

        Sentiment sentimentModel ->
            Sub.map
                SentimentMsg
                (Sentiment.subscriptions sentimentModel)


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
