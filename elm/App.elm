module App exposing (main)

import Browser.Events
import Browser.Hash as Hash
import Browser.Navigation
import Chain
import Config
import Contracts.DEthWrapper
import Dict
import ElementHelpers exposing (screenWidthToDisplayProfile)
import Eth.Sentry.Event
import Eth.Sentry.Tx
import Eth.Sentry.Wallet
import Json.Decode
import Maybe.Extra exposing (unwrap)
import Misc exposing (emptyModel, fetchAllPollsCmd, fetchApyCmd, fetchBalancerPoolFryBalance, fetchDaiPrice, fetchDerivedEthBalance, fetchDethPositionInfo, fetchEthBalance, fetchEthPrice, fetchFryPrice, fetchPermaFrostLockedTokenBalance, fetchPermaFrostTotalSupply, fetchTeamTokenBalance, fetchTreasuryBalances, userInfo)
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



-- errorPassthrough : Result Json.Decode.Error b -> Result Json.Decode.Error b
-- errorPassthrough r =
--     case r of
--         Err err ->
--             Debug.log (Json.Decode.errorToString err) Err err
--         Ok _ ->
--             r


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

        ethNodeUrl =
            Config.nodeUrl Config.ethChainId model.chainConfigs
    in
    flags.chains
        |> Json.Decode.decodeValue
            (Chain.chainDecoder flags)
        --|> errorPassthrough
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
                            |> List.map (\i -> ( i.networkId, i ))
                            |> Dict.fromList
                            |> Dict.union model.chainConfigs

                    wallet =
                        if flags.hasWallet then
                            Types.NetworkReady

                        else
                            Types.NoneDetected

                    ( newSentries, newCmds ) =
                        model.chainConfigs
                            |> Dict.map (\_ c -> startSentry c)
                            |> Dict.toList
                            |> List.map (\( k, ( sentry, cmd ) ) -> ( ( k, sentry ), cmd ))
                            |> List.unzip

                    ethCmd =
                        newCmds
                            |> List.head
                            |> Maybe.withDefault Cmd.none
                in
                ( { model
                    | chainConfigs = config
                    , route = route
                    , wallet = wallet
                    , dProfile = screenWidthToDisplayProfile Config.displayProfileBreakpoint flags.width
                    , sentries = Dict.fromList newSentries
                  }
                , (Misc.refreshCmds ethNodeUrl wallet True "" Nothing
                    ++ [ ethCmd
                       , fetchAllPollsCmd
                       , if route == Routing.Home then
                            Browser.Navigation.pushUrl
                                model.navKey
                                (Routing.routeToString model.basePath Routing.Stats)

                         else
                            Cmd.none
                       ]
                  )
                    |> Cmd.batch
                )
            )


startSentry : Types.ChainConfig -> ( Eth.Sentry.Event.EventSentry Msg, Cmd Msg )
startSentry config =
    let
        dethMintEventFilter =
            Contracts.DEthWrapper.squanderEventFilter

        ( initEventSentry, initEventSentryCmd ) =
            Eth.Sentry.Event.init
                (Types.EventSentryMsg config.networkId)
                config.nodeUrl

        ( eventSentry, secondEventSentryCmd ) =
            ( initEventSentry, initEventSentryCmd )

        -- Eth.Sentry.Event.watch
        --     (Contracts.DEthWrapper.decodeIssuedEventData
        --         >> Types.IssuedEventReceived
        --     )
        --     initEventSentry
        --     dethMintEventFilter
    in
    ( eventSentry
    , Cmd.batch [ initEventSentryCmd, secondEventSentryCmd ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    [ Time.every (1000 * 1) Types.UpdateNow
    , Time.every (1000 * 10) (always Types.RefreshAll)
    , Time.every (1000 * 150) (always Types.RefetchStakingInfoOrApy)
    , Time.every (1000 * 15) Types.Tick
    , Ports.web3SignResult Types.Web3SignResultValue
    , Ports.web3ValidateSigResult Types.Web3ValidateSigResultValue
    , Ports.walletResponse
        (Wallet.walletInfoDecoder >> Types.WalletResponse)
    , Ports.chainSwitchResponse
        (Wallet.chainSwitchDecoder >> Types.ChainSwitchResponse)
    , Ports.txSendResponse
        (Wallet.rpcResponseDecoder >> Types.TxSendResponse)
    , Browser.Events.onResize Types.Resize
    ]
        |> Sub.batch
