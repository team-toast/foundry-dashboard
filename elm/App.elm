module App exposing (main)

import Browser.Events
import Browser.Hash as Hash
import Browser.Navigation
import Chain
import Config
import ElementHelpers exposing (screenWidthToDisplayProfile)
import Eth.Net
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
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

                    ( wallet, walletNotices ) =
                        if flags.networkId == 0 then
                            ( Types.NoneDetected
                            , [ UserNotice.noWeb3Provider ]
                            )

                        else
                            ( Eth.Net.toNetworkId flags.networkId
                                |> Types.OnlyNetwork
                            , []
                            )

                    ( eventSentry, eventSentryCmd ) =
                        EventSentry.init EventSentryMsg <| Config.httpProviderUrl flags.networkId
                in
                ( { model
                    | config = config
                    , route = route
                    , wallet = wallet
                    , userNotices = walletNotices
                    , dProfile = screenWidthToDisplayProfile Config.displayProfileBreakpoint flags.width
                    , networkId = Just flags.networkId
                  }
                , [ fetchEthPrice
                  , fetchDaiPrice flags.networkId
                  , fetchFryPrice flags.networkId
                  , fetchTeamTokenBalance flags.networkId (Config.fryContractAddress flags.networkId) Config.teamToastAddress1 0
                  , fetchTeamTokenBalance flags.networkId (Config.fryContractAddress flags.networkId) Config.teamToastAddress2 1
                  , fetchTeamTokenBalance flags.networkId (Config.fryContractAddress flags.networkId) Config.teamToastAddress3 2
                  , fetchPermaFrostLockedTokenBalance flags.networkId
                  , fetchPermaFrostTotalSupply flags.networkId
                  , fetchBalancerPoolFryBalance flags.networkId
                  , fetchTreasuryBalance flags.networkId
                  , fetchAllPollsCmd
                  , model.wallet
                        |> fetchDerivedEthBalance flags.networkId
                  , model.wallet
                        |> fetchEthBalance flags.networkId
                  , fetchApyCmd flags.networkId
                  , model.withDrawalAmount
                        |> TokenValue.fromString
                        |> fetchDethPositionInfo flags.networkId
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
