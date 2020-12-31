module Stats.Types exposing (..)

import Common.Msg exposing (..)
import Config
import Contracts.BucketSale.Wrappers as BucketSaleWrappers
import Contracts.UniSwapGraph.Object exposing (..)
import Contracts.UniSwapGraph.Object.Bundle as Bundle
import Contracts.UniSwapGraph.Object.Token as Token
import Contracts.UniSwapGraph.Query as Query
import Contracts.UniSwapGraph.Scalar exposing (Id(..))
import Contracts.UniSwapGraph.ScalarCodecs exposing (..)
import Eth.Types exposing (Address)
import Eth.Utils
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Helpers.Time as TimeHelpers
import Http
import Time
import TokenValue exposing (TokenValue)


type alias Model =
    { currentTime : Int
    , currentBucketId : Maybe Int
    , currentBucketTotalEntered : TokenValue
    , currentEthPriceUsd : Float
    , currentDaiPriceEth : Float
    , currentFryPriceEth : Float
    , circSupply : Maybe Float
    , marketCap : Maybe Float
    , fullyDiluted : Maybe Float
    }


type Msg
    = MsgUp MsgUp
    | BucketValueEnteredFetched (Maybe Int) (Result Http.Error TokenValue)
    | FetchedEthPrice (Result (Graphql.Http.Error (Maybe Value)) (Maybe Value))
    | FetchedDaiPrice (Result (Graphql.Http.Error (Maybe Value)) (Maybe Value))
    | FetchedFryPrice (Result (Graphql.Http.Error (Maybe Value)) (Maybe Value))
    | Tick Time.Posix


type alias Value =
    { ethPrice : Float
    }


type alias UpdateResult =
    { newModel : Model
    , cmd : Cmd Msg
    , msgUps : List MsgUp
    }


justModelUpdate :
    Model
    -> UpdateResult
justModelUpdate model =
    { newModel = model
    , cmd = Cmd.none
    , msgUps = []
    }


resultBundle : SelectionSet Value Contracts.UniSwapGraph.Object.Bundle
resultBundle =
    SelectionSet.map Value
        (Bundle.ethPrice
            |> SelectionSet.map
                (\(Contracts.UniSwapGraph.Scalar.BigDecimal dec) ->
                    String.toFloat dec
                        |> Maybe.withDefault 0
                )
        )


resultToken : SelectionSet Value Contracts.UniSwapGraph.Object.Token
resultToken =
    SelectionSet.map Value
        (Token.derivedETH
            |> SelectionSet.map
                (\(Contracts.UniSwapGraph.Scalar.BigDecimal dec) ->
                    String.toFloat dec
                        |> Maybe.withDefault 0
                )
        )


fetchEthPrice : Cmd Msg
fetchEthPrice =
    Query.bundle identity { id = Id "1" } resultBundle
        |> Graphql.Http.queryRequest Config.uniswapGraphQL
        |> Graphql.Http.send FetchedEthPrice


fetchDaiPrice : Cmd Msg
fetchDaiPrice =
    Query.token identity
        { id =
            Id <|
                Eth.Utils.addressToString Config.daiContractAddress
        }
        resultToken
        |> Graphql.Http.queryRequest Config.uniswapGraphQL
        |> Graphql.Http.send FetchedDaiPrice


fetchFryPrice : Cmd Msg
fetchFryPrice =
    Query.token identity
        { id =
            Id <|
                Eth.Utils.addressToString Config.fryContractAddress
        }
        resultToken
        |> Graphql.Http.queryRequest Config.uniswapGraphQL
        |> Graphql.Http.send FetchedFryPrice


calcCircSupply :
    Maybe Int
    -> Maybe Float
calcCircSupply currentBucketId =
    case currentBucketId of
        Just bucketId ->
            Just <|
                (Config.bucketSaleTokensPerBucket
                    |> TokenValue.toFloatWithWarning
                )
                    * (bucketId
                        |> toFloat
                      )
                    + toFloat
                        (14553000
                            + 13645000
                            + 10000000
                            + 1800000
                        )
                    - (toFloat 1223583 * 0.588938)

        _ ->
            Nothing


calcMarketCap :
    Float
    -> Float
    -> Maybe Int
    -> Maybe Float
calcMarketCap currentFryPriceEth currentEthPriceUsd currentBucketId =
    case currentBucketId of
        Just bucketId ->
            Just <|
                (calcCircSupply currentBucketId
                    |> Maybe.withDefault 0
                )
                    * currentFryPriceEth
                    * currentEthPriceUsd

        _ ->
            Nothing


calcFullyDilutedMarketCap :
    Float
    -> Float
    -> Maybe Float
calcFullyDilutedMarketCap currentFryPriceEth currentEthPriceUsd =
    Just <|
        toFloat Config.fryTotalSupply
            * currentFryPriceEth
            * currentEthPriceUsd


getCurrentBucketId :
    Int
    -> Maybe Int
getCurrentBucketId now =
    Just <|
        (TimeHelpers.sub (Time.millisToPosix now) (Time.millisToPosix Config.saleStarted)
            |> TimeHelpers.posixToSeconds
        )
            // (Config.bucketSaleBucketInterval
                    |> TimeHelpers.posixToSeconds
               )


getBucketRemainingTimeText :
    Maybe Int
    -> Int
    -> String
getBucketRemainingTimeText bucketId now =
    case bucketId of
        Just id ->
            TimeHelpers.toHumanReadableString
                (TimeHelpers.sub
                    (getBucketEndTime id)
                    (Time.millisToPosix now)
                )

        _ ->
            "Loading..."


getBucketStartTime :
    Int
    -> Time.Posix
getBucketStartTime bucketId =
    Time.millisToPosix
        (Config.saleStarted + (bucketId * Time.posixToMillis Config.bucketSaleBucketInterval))


getBucketEndTime :
    Int
    -> Time.Posix
getBucketEndTime bucketId =
    TimeHelpers.add
        (getBucketStartTime bucketId)
        Config.bucketSaleBucketInterval


calcEffectivePricePerToken :
    TokenValue
    -> Float
    -> TokenValue
calcEffectivePricePerToken totalValueEntered tokenValue =
    let
        ve =
            totalValueEntered
                |> TokenValue.toFloatWithWarning

        tpb =
            Config.bucketSaleTokensPerBucket
                |> TokenValue.toFloatWithWarning
    in
    (ve * tokenValue / tpb)
        |> TokenValue.fromFloatWithWarning


fetchTotalValueEnteredCmd :
    Maybe Int
    -> Cmd Msg
fetchTotalValueEnteredCmd bucketId =
    case bucketId of
        Just id ->
            BucketSaleWrappers.getTotalValueEnteredForBucket
                id
                (Just id
                    |> BucketValueEnteredFetched
                )

        _ ->
            Cmd.none
