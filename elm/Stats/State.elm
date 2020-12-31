module Stats.State exposing (..)

import Common.Msg exposing (..)
import Eth.Types exposing (Address)
import Http
import Json.Decode exposing (Decoder)
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
      }
    , let
        getEthPrice =
            fetchEthPrice

        getDaiPrice =
            fetchDaiPrice

        getFryPrice =
            fetchFryPrice
      in
      Cmd.batch [ getEthPrice, getDaiPrice, getFryPrice ]
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
                            , marketCap =
                                calcMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                                    prevModel.currentBucketId
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
                            , marketCap =
                                calcMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                                    prevModel.currentBucketId
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
                            , marketCap =
                                calcMarketCap
                                    prevModel.currentFryPriceEth
                                    prevModel.currentEthPriceUsd
                                    prevModel.currentBucketId
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
            in
            UpdateResult
                { prevModel
                    | currentTime = Time.posixToMillis i
                    , currentBucketId = getCurrentBucketId <| Time.posixToMillis i
                }
                (Cmd.batch
                    [ getTotalValueEntered, getEthPrice, getDaiPrice, getFryPrice ]
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
