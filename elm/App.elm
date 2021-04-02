module App exposing (main)

import Browser.Events
import Browser.Hash as Hash
import Browser.Navigation
import Chain exposing (whenJust)
import Config
import ElementHelpers exposing (screenWidthToDisplayProfile)
import Eth.Sentry.Event
import Eth.Sentry.Tx
import Eth.Sentry.Wallet
import Json.Decode
import Maybe.Extra exposing (unwrap)
import Misc exposing (emptyModel, fetchAllPollsCmd, fetchApyCmd, fetchBalancerPoolFryBalance, fetchDaiPrice, fetchDerivedEthBalance, fetchDethPositionInfo, fetchEthBalance, fetchEthPrice, fetchFryPrice, fetchPermaFrostLockedTokenBalance, fetchPermaFrostTotalSupply, fetchTeamTokenBalance, fetchTreasuryBalance, locationCheckDecoder, userInfo)
import Ports
import Routing
import Time
import TokenValue exposing (TokenValue)
import Types exposing (Flags, Model, Msg)
import Update exposing (gotoRoute, update)
import Url exposing (Url)
import UserNotice as UN
import View exposing (view)
import Wallet


main : Program Flags Model Msg
main =
    Hash.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = Types.LinkClicked
        , onUrlChange = Types.UrlChanged
        }


init : Flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model =
            emptyModel key now flags.basePath flags.cookieConsent

        now =
            flags.nowInMillis
                |> Time.millisToPosix

        route =
            Routing.urlToRoute url
    in
    flags.chains
        |> Json.Decode.decodeValue
            (Chain.chainDecoder flags)
        |> Result.toMaybe
        |> unwrap
            ( { model
                | userNotices =
                    [ UN.unexpectedError "Config decode failure" ]
              }
            , Cmd.none
            )
            (\chainConfigs ->
                let
                    config =
                        chainConfigs
                            |> List.foldl
                                (\data ->
                                    case data.chain of
                                        Types.XDai ->
                                            \config_ ->
                                                { config_
                                                    | xDai = data
                                                }

                                        Types.Eth ->
                                            \config_ ->
                                                { config_
                                                    | ethereum = data
                                                }

                                        Types.BSC ->
                                            \config_ ->
                                                { config_
                                                    | bsc = data
                                                }
                                )
                                model.config

                    wallet =
                        if flags.hasWallet then
                            Types.NetworkReady

                        else
                            Types.NoneDetected

                    ( ethSentry, ethCmd1, ethCmd2 ) =
                        startSentry model.config.ethereum

                    ( xDaiSentry, xDaiCmd1, xDaiCmd2 ) =
                        startSentry model.config.xDai

                    ( bscSentry, bscCmd1, bscCmd2 ) =
                        startSentry model.config.bsc

                    chain =
                        model.wallet
                            |> Wallet.userInfo
                            |> whenJust
                                (\userInfo ->
                                    userInfo.chain
                                )
                in
                ( { model
                    | config = config
                    , route = route
                    , wallet = wallet
                    , dProfile = screenWidthToDisplayProfile Config.displayProfileBreakpoint flags.width
                    , sentries =
                        model.sentries
                            |> (\cs ->
                                    { cs
                                        | xDai = xDaiSentry
                                        , ethereum = ethSentry
                                        , bsc = bscSentry
                                    }
                               )
                  }
                , [ fetchEthPrice
                  , fetchDaiPrice chain
                  , fetchFryPrice chain
                  , fetchTeamTokenBalance chain (Config.fryContractAddress chain) Config.teamToastAddress1 0
                  , fetchTeamTokenBalance chain (Config.fryContractAddress chain) Config.teamToastAddress2 1
                  , fetchTeamTokenBalance chain (Config.fryContractAddress chain) Config.teamToastAddress3 2
                  , fetchPermaFrostLockedTokenBalance chain
                  , fetchPermaFrostTotalSupply chain
                  , fetchBalancerPoolFryBalance chain
                  , fetchTreasuryBalance chain
                  , fetchAllPollsCmd
                  , model.wallet
                        |> fetchDerivedEthBalance chain
                  , model.wallet
                        |> fetchEthBalance chain
                  , fetchApyCmd chain
                  , model.withDrawalAmount
                        |> TokenValue.fromString
                        |> fetchDethPositionInfo chain
                  , if route == Routing.Home then
                        Browser.Navigation.pushUrl
                            model.navKey
                            (Routing.routeToString model.basePath Routing.Stats)

                    else
                        Cmd.none
                  ]
                    |> Cmd.batch
                )
            )


startSentry : Types.ChainConfig -> ( Eth.Sentry.Event.EventSentry Msg, Cmd Msg, Cmd Msg )
startSentry config =
    let
        ( initEventSentry, initEventSentryCmd ) =
            Eth.Sentry.Event.init (Types.EventSentryMsg config.chain)
                config.providerUrl
    in
    ( initEventSentry, initEventSentryCmd, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    [ Time.every (1000 * 5) Types.UpdateNow
    , Time.every (1000 * 5) <| always Types.RefreshAll
    , Time.every (1000 * 15) (always Types.RefetchStakingInfoOrApy)
    , Time.every (1000 * 15) Types.Tick
    , Ports.locationCheckResult
        (Json.Decode.decodeValue locationCheckDecoder >> Types.LocationCheckResult)

    --- from SmokeSignal
    , Ports.web3SignResult Types.Web3SignResultValue
    , Ports.web3ValidateSigResult Types.Web3ValidateSigResultValue
    , Ports.walletSentryPort
        (Eth.Sentry.Wallet.decodeToMsg
            (Types.WalletStatus << Err)
            (Types.WalletStatus << Ok)
        )
    , Eth.Sentry.Tx.listen model.txSentry
    , Browser.Events.onResize Types.Resize
    ]
        |> Sub.batch
