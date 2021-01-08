-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Contracts.UniSwapGraph.Object.Swap exposing (..)

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


id : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.Id Contracts.UniSwapGraph.Object.Swap
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecId |> .decoder)


transaction :
    SelectionSet decodesTo Contracts.UniSwapGraph.Object.Transaction
    -> SelectionSet decodesTo Contracts.UniSwapGraph.Object.Swap
transaction object_ =
    Object.selectionForCompositeField "transaction" [] object_ identity


timestamp : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigInt Contracts.UniSwapGraph.Object.Swap
timestamp =
    Object.selectionForField "ScalarCodecs.BigInt" "timestamp" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigInt |> .decoder)


pair :
    SelectionSet decodesTo Contracts.UniSwapGraph.Object.Pair
    -> SelectionSet decodesTo Contracts.UniSwapGraph.Object.Swap
pair object_ =
    Object.selectionForCompositeField "pair" [] object_ identity


sender : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.Bytes Contracts.UniSwapGraph.Object.Swap
sender =
    Object.selectionForField "ScalarCodecs.Bytes" "sender" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBytes |> .decoder)


amount0In : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.Swap
amount0In =
    Object.selectionForField "ScalarCodecs.BigDecimal" "amount0In" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


amount1In : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.Swap
amount1In =
    Object.selectionForField "ScalarCodecs.BigDecimal" "amount1In" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


amount0Out : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.Swap
amount0Out =
    Object.selectionForField "ScalarCodecs.BigDecimal" "amount0Out" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


amount1Out : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.Swap
amount1Out =
    Object.selectionForField "ScalarCodecs.BigDecimal" "amount1Out" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


to : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.Bytes Contracts.UniSwapGraph.Object.Swap
to =
    Object.selectionForField "ScalarCodecs.Bytes" "to" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBytes |> .decoder)


logIndex : SelectionSet (Maybe Contracts.UniSwapGraph.ScalarCodecs.BigInt) Contracts.UniSwapGraph.Object.Swap
logIndex =
    Object.selectionForField "(Maybe ScalarCodecs.BigInt)" "logIndex" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigInt |> .decoder |> Decode.nullable)


amountUSD : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.Swap
amountUSD =
    Object.selectionForField "ScalarCodecs.BigDecimal" "amountUSD" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)
