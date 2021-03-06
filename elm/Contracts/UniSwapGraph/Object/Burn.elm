-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Contracts.UniSwapGraph.Object.Burn exposing (..)

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


id : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.Id Contracts.UniSwapGraph.Object.Burn
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecId |> .decoder)


transaction :
    SelectionSet decodesTo Contracts.UniSwapGraph.Object.Transaction
    -> SelectionSet decodesTo Contracts.UniSwapGraph.Object.Burn
transaction object_ =
    Object.selectionForCompositeField "transaction" [] object_ identity


timestamp : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigInt Contracts.UniSwapGraph.Object.Burn
timestamp =
    Object.selectionForField "ScalarCodecs.BigInt" "timestamp" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigInt |> .decoder)


pair :
    SelectionSet decodesTo Contracts.UniSwapGraph.Object.Pair
    -> SelectionSet decodesTo Contracts.UniSwapGraph.Object.Burn
pair object_ =
    Object.selectionForCompositeField "pair" [] object_ identity


liquidity : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigDecimal Contracts.UniSwapGraph.Object.Burn
liquidity =
    Object.selectionForField "ScalarCodecs.BigDecimal" "liquidity" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder)


sender : SelectionSet (Maybe Contracts.UniSwapGraph.ScalarCodecs.Bytes) Contracts.UniSwapGraph.Object.Burn
sender =
    Object.selectionForField "(Maybe ScalarCodecs.Bytes)" "sender" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBytes |> .decoder |> Decode.nullable)


amount0 : SelectionSet (Maybe Contracts.UniSwapGraph.ScalarCodecs.BigDecimal) Contracts.UniSwapGraph.Object.Burn
amount0 =
    Object.selectionForField "(Maybe ScalarCodecs.BigDecimal)" "amount0" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder |> Decode.nullable)


amount1 : SelectionSet (Maybe Contracts.UniSwapGraph.ScalarCodecs.BigDecimal) Contracts.UniSwapGraph.Object.Burn
amount1 =
    Object.selectionForField "(Maybe ScalarCodecs.BigDecimal)" "amount1" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder |> Decode.nullable)


to : SelectionSet (Maybe Contracts.UniSwapGraph.ScalarCodecs.Bytes) Contracts.UniSwapGraph.Object.Burn
to =
    Object.selectionForField "(Maybe ScalarCodecs.Bytes)" "to" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBytes |> .decoder |> Decode.nullable)


logIndex : SelectionSet (Maybe Contracts.UniSwapGraph.ScalarCodecs.BigInt) Contracts.UniSwapGraph.Object.Burn
logIndex =
    Object.selectionForField "(Maybe ScalarCodecs.BigInt)" "logIndex" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigInt |> .decoder |> Decode.nullable)


amountUSD : SelectionSet (Maybe Contracts.UniSwapGraph.ScalarCodecs.BigDecimal) Contracts.UniSwapGraph.Object.Burn
amountUSD =
    Object.selectionForField "(Maybe ScalarCodecs.BigDecimal)" "amountUSD" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder |> Decode.nullable)


needsComplete : SelectionSet Bool Contracts.UniSwapGraph.Object.Burn
needsComplete =
    Object.selectionForField "Bool" "needsComplete" [] Decode.bool


feeTo : SelectionSet (Maybe Contracts.UniSwapGraph.ScalarCodecs.Bytes) Contracts.UniSwapGraph.Object.Burn
feeTo =
    Object.selectionForField "(Maybe ScalarCodecs.Bytes)" "feeTo" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBytes |> .decoder |> Decode.nullable)


feeLiquidity : SelectionSet (Maybe Contracts.UniSwapGraph.ScalarCodecs.BigDecimal) Contracts.UniSwapGraph.Object.Burn
feeLiquidity =
    Object.selectionForField "(Maybe ScalarCodecs.BigDecimal)" "feeLiquidity" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigDecimal |> .decoder |> Decode.nullable)
