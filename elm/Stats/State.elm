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
      , currentBucketId = getCurrentBucketId nowInMillis
      , currentBucketTotalEntered = TokenValue.fromIntTokenValue 0
      , currentEthPriceUsd = 0.0
      , currentDaiPriceEth = 0.0
      , currentFryPriceEth = 0.0
      , circSupply = Just 0.0
      , marketCap = Just 0.0
      , fullyDiluted = Just 0.0
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
                            | currentEthPriceUsd = v.ethPrice
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
                            | currentDaiPriceEth = v.ethPrice
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
                            | currentFryPriceEth = v.ethPrice
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
                        { prevModel | currentBucketTotalEntered = valueEntered }
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
