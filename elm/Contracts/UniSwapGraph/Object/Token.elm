-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Contracts.UniSwapGraph.Object.Token exposing (..)

import Contracts.UniSwapGraph.Enum.OrderDirection
import Contracts.UniSwapGraph.Enum.PairDayData_orderBy
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


id : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.Id Contracts.UniSwapGraph.Object.Token
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecId |> .decoder)


symbol : SelectionSet String Contracts.UniSwapGraph.Object.Token
symbol =
    Object.selectionForField "String" "symbol" [] Decode.string


name : SelectionSet String Contracts.UniSwapGraph.Object.Token
name =
    Object.selectionForField "String" "name" [] Decode.string


decimals : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigInt Contracts.UniSwapGraph.Object.Token
decimals =
    Object.selectionForField "ScalarCodecs.BigInt" "decimals" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigInt |> .decoder)


totalSupply : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigInt Contracts.UniSwapGraph.Object.Token
totalSupply =
    Object.selectionForField "ScalarCodecs.BigInt" "totalSupply" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigInt |> .decoder)


tradeVolume : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.Token
tradeVolume =
    Object.selectionForField "ScalarCodecs.BigDecimal" "tradeVolume" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


tradeVolumeUSD : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.Token
tradeVolumeUSD =
    Object.selectionForField "ScalarCodecs.BigDecimal" "tradeVolumeUSD" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


untrackedVolumeUSD : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.Token
untrackedVolumeUSD =
    Object.selectionForField "ScalarCodecs.BigDecimal" "untrackedVolumeUSD" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


txCount : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigInt Contracts.UniSwapGraph.Object.Token
txCount =
    Object.selectionForField "ScalarCodecs.BigInt" "txCount" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigInt |> .decoder)


totalLiquidity : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.Token
totalLiquidity =
    Object.selectionForField "ScalarCodecs.BigDecimal" "totalLiquidity" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


derivedETH : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.Token
derivedETH =
    Object.selectionForField "ScalarCodecs.BigDecimal" "derivedETH" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


type alias MostLiquidPairsOptionalArguments =
    { skip : OptionalArgument Int
    , first : OptionalArgument Int
    , orderBy : OptionalArgument Contracts.UniSwapGraph.Enum.PairDayData_orderBy.PairDayData_orderBy
    , orderDirection : OptionalArgument Contracts.UniSwapGraph.Enum.OrderDirection.OrderDirection
    , where_ : OptionalArgument Contracts.UniSwapGraph.InputObject.PairDayData_filter
    }


mostLiquidPairs :
    (MostLiquidPairsOptionalArguments -> MostLiquidPairsOptionalArguments)
    -> SelectionSet decodesTo Contracts.UniSwapGraph.Object.PairDayData
    -> SelectionSet (List (Maybe decodesTo)) Contracts.UniSwapGraph.Object.Token
mostLiquidPairs fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { skip = Absent, first = Absent, orderBy = Absent, orderDirection = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "skip" filledInOptionals.skip Encode.int, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "orderBy" filledInOptionals.orderBy (Encode.enum Contracts.UniSwapGraph.Enum.PairDayData_orderBy.toString), Argument.optional "orderDirection" filledInOptionals.orderDirection (Encode.enum Contracts.UniSwapGraph.Enum.OrderDirection.toString), Argument.optional "where" filledInOptionals.where_ Contracts.UniSwapGraph.InputObject.encodePairDayData_filter ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "mostLiquidPairs" optionalArgs object_ (identity >> Decode.nullable >> Decode.list)
