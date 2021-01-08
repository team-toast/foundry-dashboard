-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Contracts.UniSwapGraph.Object.UniswapFactory exposing (..)

import Contracts.UniSwapGraph.Enum.OrderDirection
import Contracts.UniSwapGraph.Enum.TokenDayData_orderBy
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


id : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.Id Contracts.UniSwapGraph.Object.UniswapFactory
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecId |> .decoder)


pairCount : SelectionSet Int Contracts.UniSwapGraph.Object.UniswapFactory
pairCount =
    Object.selectionForField "Int" "pairCount" [] Decode.int


totalVolumeUSD : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.UniswapFactory
totalVolumeUSD =
    Object.selectionForField "ScalarCodecs.BigDecimal" "totalVolumeUSD" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


totalVolumeETH : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.UniswapFactory
totalVolumeETH =
    Object.selectionForField "ScalarCodecs.BigDecimal" "totalVolumeETH" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


untrackedVolumeUSD : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.UniswapFactory
untrackedVolumeUSD =
    Object.selectionForField "ScalarCodecs.BigDecimal" "untrackedVolumeUSD" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


totalLiquidityUSD : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.UniswapFactory
totalLiquidityUSD =
    Object.selectionForField "ScalarCodecs.BigDecimal" "totalLiquidityUSD" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


totalLiquidityETH : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.UniswapFactory
totalLiquidityETH =
    Object.selectionForField "ScalarCodecs.BigDecimal" "totalLiquidityETH" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


txCount : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigInt Contracts.UniSwapGraph.Object.UniswapFactory
txCount =
    Object.selectionForField "ScalarCodecs.BigInt" "txCount" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigInt |> .decoder)


type alias MostLiquidTokensOptionalArguments =
    { skip : OptionalArgument Int
    , first : OptionalArgument Int
    , orderBy : OptionalArgument Contracts.UniSwapGraph.Enum.TokenDayData_orderBy.TokenDayData_orderBy
    , orderDirection : OptionalArgument Contracts.UniSwapGraph.Enum.OrderDirection.OrderDirection
    , where_ : OptionalArgument Contracts.UniSwapGraph.InputObject.TokenDayData_filter
    }


mostLiquidTokens :
    (MostLiquidTokensOptionalArguments -> MostLiquidTokensOptionalArguments)
    -> SelectionSet decodesTo Contracts.UniSwapGraph.Object.TokenDayData
    -> SelectionSet (List decodesTo) Contracts.UniSwapGraph.Object.UniswapFactory
mostLiquidTokens fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { skip = Absent, first = Absent, orderBy = Absent, orderDirection = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "skip" filledInOptionals.skip Encode.int, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "orderBy" filledInOptionals.orderBy (Encode.enum Contracts.UniSwapGraph.Enum.TokenDayData_orderBy.toString), Argument.optional "orderDirection" filledInOptionals.orderDirection (Encode.enum Contracts.UniSwapGraph.Enum.OrderDirection.toString), Argument.optional "where" filledInOptionals.where_ Contracts.UniSwapGraph.InputObject.encodeTokenDayData_filter ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "mostLiquidTokens" optionalArgs object_ (identity >> Decode.list)
