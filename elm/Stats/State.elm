module Stats.State exposing (..)

import Array
import Common.Msg exposing (..)
import Config
import Eth.Types exposing (Address)
import Http
import Json.Decode exposing (Decoder, value)
import Stats.Types exposing (..)
import Time
import TokenValue


init :
    Int
    -> ( Model, Cmd Msg )
init nowInMillis =
    ( { currentTime = nowInMillis
      , currentBucketId = Nothing
      , currentBucketTotalEntered = Nothing
      , currentEthPriceUsd = Nothing
      , currentDaiPriceEth = Nothing
      , currentFryPriceEth = Nothing
      , circSupply = Nothing
      , marketCap = Nothing
      , fullyDiluted = Nothing
      , permaFrostedTokens = Nothing
      , teamTokenBalances = Array.initialize 3 (always Nothing)
      , balancerFryBalance = Nothing
      , permaFrostTotalSupply = Nothing
      , permaFrostBalanceLocked = Nothing
      , treasuryBalance = Nothing
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
      Cmd.batch
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
        ]
    )


update :
    Msg
    -> Model
    -> UpdateResult
update msg prevModel =
    case msg of
        MsgUp msgUp ->
            UpdateResult
                prevModel
                Cmd.none
                [ msgUp ]

        FetchedEthPrice fetchResult ->
            case fetchResult of
                Err error ->
                    -- let
                    --     _ =
                    --         Debug.log "GraphQL error" ( fetchResult, error )
                    -- in
                    UpdateResult
                        prevModel
                        Cmd.none
                        []

                Ok bundle1 ->
                    let
                        v =
                            case bundle1 of
                                Just val ->
                                    val

                                Nothing ->
                                    Value 0
                    in
                    UpdateResult
                        { prevModel
                            | currentEthPriceUsd =
                                Just
                                    v.ethPrice
                            , circSupply =
                                calcCircSupply
                                    prevModel.currentBucketId
                                    prevModel.teamTokenBalances
                                    prevModel.permaFrostedTokens
                            , marketCap =
                                calcMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                                    prevModel.circSupply
                            , fullyDiluted =
                                calcFullyDilutedMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                        }
                        Cmd.none
                        []

        FetchedDaiPrice fetchResult ->
            case fetchResult of
                Err error ->
                    -- let
                    --     _ =
                    --         Debug.log "GraphQL error" ( fetchResult, error )
                    -- in
                    UpdateResult
                        prevModel
                        Cmd.none
                        []

                Ok value ->
                    let
                        v =
                            case value of
                                Just val ->
                                    val

                                Nothing ->
                                    Value 0
                    in
                    UpdateResult
                        { prevModel
                            | currentDaiPriceEth =
                                Just
                                    v.ethPrice
                            , circSupply =
                                calcCircSupply
                                    prevModel.currentBucketId
                                    prevModel.teamTokenBalances
                                    prevModel.permaFrostedTokens
                            , marketCap =
                                calcMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                                    prevModel.circSupply
                            , fullyDiluted =
                                calcFullyDilutedMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                        }
                        Cmd.none
                        []

        FetchedFryPrice fetchResult ->
            case fetchResult of
                Err error ->
                    -- let
                    --     _ =
                    --         Debug.log "GraphQL error" ( fetchResult, error )
                    -- in
                    UpdateResult
                        prevModel
                        Cmd.none
                        []

                Ok value ->
                    let
                        v =
                            case value of
                                Just val ->
                                    val

                                Nothing ->
                                    Value 0
                    in
                    UpdateResult
                        { prevModel
                            | currentFryPriceEth =
                                Just
                                    v.ethPrice
                            , circSupply =
                                calcCircSupply
                                    prevModel.currentBucketId
                                    prevModel.teamTokenBalances
                                    prevModel.permaFrostedTokens
                            , marketCap =
                                calcMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                                    prevModel.circSupply
                            , fullyDiluted =
                                calcFullyDilutedMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                        }
                        Cmd.none
                        []

        FetchedTeamTokens index fetchResult ->
            case fetchResult of
                Err error ->
                    -- let
                    --     _ =
                    --         Debug.log "GraphQL error" ( fetchResult, error )
                    -- in
                    UpdateResult
                        prevModel
                        Cmd.none
                        []

                Ok value ->
                    UpdateResult
                        { prevModel
                            | teamTokenBalances = Array.set index (Just value) prevModel.teamTokenBalances
                            , circSupply =
                                calcCircSupply
                                    prevModel.currentBucketId
                                    prevModel.teamTokenBalances
                                    prevModel.permaFrostedTokens
                            , marketCap =
                                calcMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                                    prevModel.circSupply
                            , fullyDiluted =
                                calcFullyDilutedMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                        }
                        Cmd.none
                        []

        FetchedBalancerFryBalance fetchResult ->
            case fetchResult of
                Err error ->
                    -- let
                    --     _ =
                    --         Debug.log "GraphQL error" ( fetchResult, error )
                    -- in
                    UpdateResult
                        prevModel
                        Cmd.none
                        []

                Ok value ->
                    UpdateResult
                        { prevModel
                            | balancerFryBalance = Just value
                            , permaFrostedTokens =
                                calcPermaFrostedTokens
                                    (Just value)
                                    prevModel.permaFrostBalanceLocked
                                    prevModel.permaFrostTotalSupply
                            , circSupply =
                                calcCircSupply
                                    prevModel.currentBucketId
                                    prevModel.teamTokenBalances
                                    prevModel.permaFrostedTokens
                            , marketCap =
                                calcMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                                    prevModel.circSupply
                            , fullyDiluted =
                                calcFullyDilutedMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                        }
                        Cmd.none
                        []

        FetchedPermaFrostBalanceLocked fetchResult ->
            case fetchResult of
                Err error ->
                    -- let
                    --     _ =
                    --         Debug.log "GraphQL error" ( fetchResult, error )
                    -- in
                    UpdateResult
                        prevModel
                        Cmd.none
                        []

                Ok value ->
                    UpdateResult
                        { prevModel
                            | permaFrostBalanceLocked = Just value
                            , permaFrostedTokens =
                                calcPermaFrostedTokens
                                    prevModel.balancerFryBalance
                                    (Just value)
                                    prevModel.permaFrostTotalSupply
                            , circSupply =
                                calcCircSupply
                                    prevModel.currentBucketId
                                    prevModel.teamTokenBalances
                                    prevModel.permaFrostedTokens
                            , marketCap =
                                calcMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                                    prevModel.circSupply
                            , fullyDiluted =
                                calcFullyDilutedMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                        }
                        Cmd.none
                        []

        FetchedPermaFrostTotalSupply fetchResult ->
            case fetchResult of
                Err error ->
                    -- let
                    --     _ =
                    --         Debug.log "GraphQL error" ( fetchResult, error )
                    -- in
                    UpdateResult
                        prevModel
                        Cmd.none
                        []

                Ok value ->
                    UpdateResult
                        { prevModel
                            | permaFrostTotalSupply = Just value
                            , permaFrostedTokens =
                                calcPermaFrostedTokens
                                    prevModel.balancerFryBalance
                                    prevModel.permaFrostBalanceLocked
                                    (Just value)
                            , circSupply =
                                calcCircSupply
                                    prevModel.currentBucketId
                                    prevModel.teamTokenBalances
                                    prevModel.permaFrostedTokens
                            , marketCap =
                                calcMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                                    prevModel.circSupply
                            , fullyDiluted =
                                calcFullyDilutedMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                        }
                        Cmd.none
                        []

        BucketValueEnteredFetched bucketId fetchResult ->
            case fetchResult of
                Err httpErr ->
                    -- let
                    --     _ =
                    --         Debug.log "http error when fetching total bucket value entered" ( bucketId, fetchResult )
                    -- in
                    UpdateResult
                        prevModel
                        Cmd.none
                        []

                Ok valueEntered ->
                    UpdateResult
                        { prevModel
                            | currentBucketTotalEntered =
                                Just
                                    valueEntered
                        }
                        Cmd.none
                        []

        FetchedTreasuryBalance fetchResult ->
            case fetchResult of
                Err httpErr ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        []

                Ok valueFetched ->
                    UpdateResult
                        { prevModel
                            | treasuryBalance =
                                calcTreasuryBalance
                                    prevModel.currentDaiPriceEth
                                    prevModel.currentEthPriceUsd
                                    (Just valueFetched)
                        }
                        Cmd.none
                        []

        Tick i ->
            let
                getTotalValueEntered =
                    fetchTotalValueEnteredCmd prevModel.currentBucketId

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
            UpdateResult
                { prevModel
                    | currentTime = Time.posixToMillis i
                    , currentBucketId = getCurrentBucketId <| Time.posixToMillis i
                }
                (Cmd.batch
                    [ getTotalValueEntered
                    , getEthPrice
                    , getDaiPrice
                    , getFryPrice
                    , getTeamToast1
                    , getTeamToast2
                    , getTeamToast3
                    , getPermaFrostTokenBalance
                    , getPermaFrostTotalSupply
                    , getBalancerFryBalance
                    , getTreasuryBalance
                    ]
                )
                []


runMsgDown :
    MsgDown
    -> Model
    -> UpdateResult
runMsgDown msg prevModel =
    case msg of
        UpdateWallet _ ->
            justModelUpdate prevModel


subscriptions :
    Model
    -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 5000 Tick ]
