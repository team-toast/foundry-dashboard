module Stats.State exposing (..)

import Common.Msg exposing (..)
import Eth.Types exposing (Address)
import Http
import Json.Decode exposing (Decoder)
import Stats.Types exposing (..)
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
      , circSupply = 0.0
      , marketCap = 0.0
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
                            , circSupply = calcCircSupply prevModel
                            , marketCap = calcMarketCap prevModel
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
                            , circSupply = calcCircSupply prevModel
                            , marketCap = calcMarketCap prevModel
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
                            , circSupply = calcCircSupply prevModel
                            , marketCap = calcMarketCap prevModel
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
    Sub.none
