module App exposing (main)

import Browser.Events
import Browser.Hash as Hash
import Browser.Navigation
import Config
import Eth.Net
import Eth.Sentry.Event exposing (EventSentry)
import Eth.Sentry.Tx
import Eth.Sentry.Wallet
import Json.Decode
import Misc exposing (emptyModel, fetchAllPollsCmd, fetchBalancerPoolFryBalance, fetchDaiPrice, fetchEthPrice, fetchFryPrice, fetchPermaFrostLockedTokenBalance, fetchPermaFrostTotalSupply, fetchTeamTokenBalance, fetchTreasuryBalance, locationCheckDecoder)
import Ports
import Routing
import Time
import Types exposing (..)
import Update
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
            emptyModel key now flags.basePath

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
    in
    ( { model
        | route = route
        , wallet = wallet
        , userNotices = walletNotices
      }
    , let
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
      [ getEthPrice
      , getDaiPrice
      , getFryPrice
      , getTeamToast1
      , getTeamToast2
      , getTeamToast3
      , getPermaFrostTokenBalance
      , getPermaFrostTotalSupply
      , getBalancerFryBalance
      , getTreasuryBalance
      , fetchAllPollsCmd
      ]
        |> Cmd.batch
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    [ Time.every 50 UpdateNow
    , Time.every (1000 * 4) <| always RefreshAll
    , Time.every (1000 * 10) (always RefetchStakingInfoOrApy)
    , Time.every (1000 * 5) Tick
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
