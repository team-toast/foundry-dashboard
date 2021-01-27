module App exposing (main)

import Browser.Hash as Hash
import Browser.Navigation
import Config
import Eth.Sentry.Event exposing (EventSentry)
import Json.Decode
import Misc exposing (emptyModel, fetchAllPollsCmd, fetchBalancerPoolFryBalance, fetchDaiPrice, fetchEthPrice, fetchFryPrice, fetchPermaFrostLockedTokenBalance, fetchPermaFrostTotalSupply, fetchTeamTokenBalance, fetchTreasuryBalance, locationCheckDecoder)
import Ports
import Routing
import Time
import Types exposing (..)
import Update
import Url exposing (Url)
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

        model2 =
            { model
                | route = route
            }
    in
    ( model2
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
    , Ports.locationCheckResult
        (Json.Decode.decodeValue locationCheckDecoder >> LocationCheckResult)
    , Time.every 10000 (always RefetchStakingInfoOrApy)
    , Time.every 5000 Tick
    , Time.every 4000 <| always RefreshAll
    , Ports.web3SignResult Web3SignResultValue
    , Ports.web3ValidateSigResult Web3ValidateSigResultValue
    ]
        |> Sub.batch
