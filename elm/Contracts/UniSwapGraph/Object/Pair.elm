-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Contracts.UniSwapGraph.Object.Pair exposing (..)

import Contracts.UniSwapGraph.InputObject
import Contracts.UniSwapGraph.Interface
import Contracts.UniSwapGraph.Object
import Contracts.UniSwapGraph.Scalar
import Contracts.UniSwapGraph.ScalarCodecs
import Contracts.UniSwapGraph.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


id : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.Id Contracts.UniSwapGraph.Object.Pair
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecId |> .decoder)


token0 :
    SelectionSet decodesTo Contracts.UniSwapGraph.Object.Token
    -> SelectionSet decodesTo Contracts.UniSwapGraph.Object.Pair
token0 object_ =
    Object.selectionForCompositeField "token0" [] object_ identity


token1 :
    SelectionSet decodesTo Contracts.UniSwapGraph.Object.Token
    -> SelectionSet decodesTo Contracts.UniSwapGraph.Object.Pair
token1 object_ =
    Object.selectionForCompositeField "token1" [] object_ identity


reserve0 : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.Pair
reserve0 =
    Object.selectionForField "ScalarCodecs.BigDecimal" "reserve0" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


reserve1 : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.Pair
reserve1 =
    Object.selectionForField "ScalarCodecs.BigDecimal" "reserve1" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


totalSupply : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.Pair
totalSupply =
    Object.selectionForField "ScalarCodecs.BigDecimal" "totalSupply" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


reserveETH : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.Pair
reserveETH =
    Object.selectionForField "ScalarCodecs.BigDecimal" "reserveETH" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


reserveUSD : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.Pair
reserveUSD =
    Object.selectionForField "ScalarCodecs.BigDecimal" "reserveUSD" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


trackedReserveETH : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.Pair
trackedReserveETH =
    Object.selectionForField "ScalarCodecs.BigDecimal" "trackedReserveETH" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


token0Price : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.Pair
token0Price =
    Object.selectionForField "ScalarCodecs.BigDecimal" "token0Price" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


token1Price : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.Pair
token1Price =
    Object.selectionForField "ScalarCodecs.BigDecimal" "token1Price" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


volumeToken0 : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.Pair
volumeToken0 =
    Object.selectionForField "ScalarCodecs.BigDecimal" "volumeToken0" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


volumeToken1 : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.Pair
volumeToken1 =
    Object.selectionForField "ScalarCodecs.BigDecimal" "volumeToken1" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


volumeUSD : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.Pair
volumeUSD =
    Object.selectionForField "ScalarCodecs.BigDecimal" "volumeUSD" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


untrackedVolumeUSD : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.Pair
untrackedVolumeUSD =
    Object.selectionForField "ScalarCodecs.BigDecimal" "untrackedVolumeUSD" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


txCount : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigInt Contracts.UniSwapGraph.Object.Pair
txCount =
    Object.selectionForField "ScalarCodecs.BigInt" "txCount" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigInt |> .decoder)


createdAtTimestamp : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigInt Contracts.UniSwapGraph.Object.Pair
createdAtTimestamp =
    Object.selectionForField "ScalarCodecs.BigInt" "createdAtTimestamp" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigInt |> .decoder)


createdAtBlockNumber : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigInt Contracts.UniSwapGraph.Object.Pair
createdAtBlockNumber =
    Object.selectionForField "ScalarCodecs.BigInt" "createdAtBlockNumber" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigInt |> .decoder)


liquidityProviderCount : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigInt Contracts.UniSwapGraph.Object.Pair
liquidityProviderCount =
    Object.selectionForField "ScalarCodecs.BigInt" "liquidityProviderCount" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigInt |> .decoder)
