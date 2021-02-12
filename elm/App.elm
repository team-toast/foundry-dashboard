module App exposing (main)

import Browser.Events
import Browser.Hash as Hash
import Browser.Navigation
import Config
import ElementHelpers exposing (screenWidthToDisplayProfile)
import Eth.Net
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Sentry.Tx
import Eth.Sentry.Wallet
import Json.Decode
import Misc exposing (emptyModel, fetchAllPollsCmd, fetchBalancerPoolFryBalance, fetchDaiPrice, fetchDerivedEthBalance, fetchDethPositionInfo, fetchEthBalance, fetchEthPrice, fetchFryPrice, fetchPermaFrostLockedTokenBalance, fetchPermaFrostTotalSupply, fetchTeamTokenBalance, fetchTreasuryBalance, locationCheckDecoder, userInfo)
import Ports
import Routing
import Time
import TokenValue exposing (TokenValue)
import Types exposing (..)
import Update exposing (gotoRoute)
import Url exposing (Url)
import UserNotice
import View


main : Program Flags Model Msg
main =
    Hash.application
        { init = init
        , view = View.view
        , update = Update.update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
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
            EventSentry.init EventSentryMsg Config.httpProviderUrl
    in
    ( { model
        | route = route
        , wallet = wallet
        , userNotices = walletNotices
        , dProfile = screenWidthToDisplayProfile Config.displayProfileBreakpoint flags.width
      }
    , [ fetchEthPrice
      , fetchDaiPrice
      , fetchFryPrice
      , fetchTeamTokenBalance Config.fryContractAddress Config.teamToastAddress1 0
      , fetchTeamTokenBalance Config.fryContractAddress Config.teamToastAddress2 1
      , fetchTeamTokenBalance Config.fryContractAddress Config.teamToastAddress3 2
      , fetchPermaFrostLockedTokenBalance
      , fetchPermaFrostTotalSupply
      , fetchBalancerPoolFryBalance
      , fetchTreasuryBalance
      , fetchAllPollsCmd
      , model.wallet
            |> fetchDerivedEthBalance
      , model.wallet
            |> fetchEthBalance
      , model.withDrawalAmount
            |> TokenValue.fromString
            |> fetchDethPositionInfo
      , if route == Routing.Home then
            Browser.Navigation.pushUrl
                model.navKey
                (Routing.routeToString model.basePath Routing.Stats)

        else
            Cmd.none
      ]
        |> Cmd.batch
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    [ Time.every 50 UpdateNow
    , Time.every (1000 * 4) <| always RefreshAll
    , Time.every (1000 * 1) (always RefetchStakingInfoOrApy)
    , Time.every (1000 * 1) Tick
    , Ports.locationCheckResult
        (Json.Decode.decodeValue locationCheckDecoder >> LocationCheckResult)

    --- from SmokeSignal
    , Ports.web3SignResult Web3SignResultValue
    , Ports.web3ValidateSigResult Web3ValidateSigResultValue
    , Ports.walletSentryPort
        (Eth.Sentry.Wallet.decodeToMsg
            (Types.WalletStatus << Err)
            (Types.WalletStatus << Ok)
        )
    , Eth.Sentry.Tx.listen model.txSentry
    , Browser.Events.onResize Types.Resize
    ]
        |> Sub.batch
