module Stats.Types exposing (..)

import Common.Msg exposing (..)
import Config
import Contracts.BucketSale.Wrappers as BucketSaleWrappers
import Contracts.ERC20Wrapper as ERC20
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
    , currentBucketTotalEntered : Maybe TokenValue
    , currentEthPriceUsd : Maybe Float
    , currentDaiPriceEth : Maybe Float
    , currentFryPriceEth : Maybe Float
    , circSupply : Maybe Float
    , marketCap : Maybe Float
    , fullyDiluted : Maybe Float
    , teamTokens : Maybe TokenValue
    , permaFrostedTokens : Maybe TokenValue
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


loadingText : String
loadingText =
    "Loading..."


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


fetchAddressTokenBalance :
    Address
    -> Address
    -> TokenValue
fetchAddressTokenBalance tokenAddress owner =
    TokenValue.zero


calcCircSupply :
    Maybe Int
    -> Maybe TokenValue
    -> Maybe TokenValue
    -> Maybe Float
calcCircSupply currentBucketId totalTeamTokens totalPermaFrostedTokens =
    case currentBucketId of
        Just bucketId ->
            case totalTeamTokens of
                Just ttt ->
                    case totalPermaFrostedTokens of
                        Just tpt ->
                            Just <|
                                toFloat
                                    (Config.bucketSaleTokensPerBucket
                                        * bucketId
                                    )
                                    + TokenValue.toFloatWithWarning ttt
                                    - TokenValue.toFloatWithWarning tpt

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


calcMarketCap :
    Maybe Float
    -> Maybe Float
    -> Maybe Float
    -> Maybe Float
calcMarketCap currentFryPriceEth currentEthPriceUsd circSupply =
    case circSupply of
        Just cs ->
            case currentFryPriceEth of
                Just fry ->
                    case currentEthPriceUsd of
                        Just eth ->
                            cs
                                * toFloat Config.fryTotalSupply
                                * fry
                                * eth
                                |> Just

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


calcFullyDilutedMarketCap :
    Maybe Float
    -> Maybe Float
    -> Maybe Float
calcFullyDilutedMarketCap currentFryPriceEth currentEthPriceUsd =
    case currentFryPriceEth of
        Just fry ->
            case currentEthPriceUsd of
                Just eth ->
                    Just <|
                        toFloat Config.fryTotalSupply
                            * fry
                            * eth

                _ ->
                    Nothing

        _ ->
            Nothing


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
            loadingText


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
    Maybe TokenValue
    -> Maybe Float
    -> Maybe Float
    -> String
calcEffectivePricePerToken totalValueEntered tokenValueEth ethValueUsd =
    let
        tpb =
            toFloat <|
                if Config.bucketSaleTokensPerBucket < 1 then
                    1

                else
                    Config.bucketSaleTokensPerBucket
    in
    case totalValueEntered of
        Just tve ->
            case maybeFloatMultiply tokenValueEth ethValueUsd of
                Just val ->
                    tve
                        |> TokenValue.mulFloatWithWarning val
                        |> TokenValue.divFloatWithWarning tpb
                        |> TokenValue.toConciseString

                _ ->
                    loadingText

        _ ->
            loadingText


maybeFloatMultiply :
    Maybe Float
    -> Maybe Float
    -> Maybe Float
maybeFloatMultiply val1 val2 =
    case val1 of
        Just v1 ->
            case val2 of
                Just v2 ->
                    Just <| v1 * v2

                _ ->
                    Nothing

        _ ->
            Nothing


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
