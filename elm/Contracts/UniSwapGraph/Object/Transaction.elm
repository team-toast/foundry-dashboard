-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Contracts.UniSwapGraph.Object.Transaction exposing (..)

import Contracts.UniSwapGraph.Enum.Burn_orderBy
import Contracts.UniSwapGraph.Enum.Mint_orderBy
import Contracts.UniSwapGraph.Enum.OrderDirection
import Contracts.UniSwapGraph.Enum.Swap_orderBy
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


id : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.Id Contracts.UniSwapGraph.Object.Transaction
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecId |> .decoder)


blockNumber : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigInt Contracts.UniSwapGraph.Object.Transaction
blockNumber =
    Object.selectionForField "ScalarCodecs.BigInt" "blockNumber" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigInt |> .decoder)


timestamp : SelectionSet Contracts.UniSwapGraph.ScalarCodecs.BigInt Contracts.UniSwapGraph.Object.Transaction
timestamp =
    Object.selectionForField "ScalarCodecs.BigInt" "timestamp" [] (Contracts.UniSwapGraph.ScalarCodecs.codecs |> Contracts.UniSwapGraph.Scalar.unwrapCodecs |> .codecBigInt |> .decoder)


type alias MintsOptionalArguments =
    { skip : OptionalArgument Int
    , first : OptionalArgument Int
    , orderBy : OptionalArgument Contracts.UniSwapGraph.Enum.Mint_orderBy.Mint_orderBy
    , orderDirection : OptionalArgument Contracts.UniSwapGraph.Enum.OrderDirection.OrderDirection
    , where_ : OptionalArgument Contracts.UniSwapGraph.InputObject.Mint_filter
    }


mints :
    (MintsOptionalArguments -> MintsOptionalArguments)
    -> SelectionSet decodesTo Contracts.UniSwapGraph.Object.Mint
    -> SelectionSet (List (Maybe decodesTo)) Contracts.UniSwapGraph.Object.Transaction
mints fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { skip = Absent, first = Absent, orderBy = Absent, orderDirection = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "skip" filledInOptionals.skip Encode.int, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "orderBy" filledInOptionals.orderBy (Encode.enum Contracts.UniSwapGraph.Enum.Mint_orderBy.toString), Argument.optional "orderDirection" filledInOptionals.orderDirection (Encode.enum Contracts.UniSwapGraph.Enum.OrderDirection.toString), Argument.optional "where" filledInOptionals.where_ Contracts.UniSwapGraph.InputObject.encodeMint_filter ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "mints" optionalArgs object_ (identity >> Decode.nullable >> Decode.list)


type alias BurnsOptionalArguments =
    { skip : OptionalArgument Int
    , first : OptionalArgument Int
    , orderBy : OptionalArgument Contracts.UniSwapGraph.Enum.Burn_orderBy.Burn_orderBy
    , orderDirection : OptionalArgument Contracts.UniSwapGraph.Enum.OrderDirection.OrderDirection
    , where_ : OptionalArgument Contracts.UniSwapGraph.InputObject.Burn_filter
    }


burns :
    (BurnsOptionalArguments -> BurnsOptionalArguments)
    -> SelectionSet decodesTo Contracts.UniSwapGraph.Object.Burn
    -> SelectionSet (List (Maybe decodesTo)) Contracts.UniSwapGraph.Object.Transaction
burns fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { skip = Absent, first = Absent, orderBy = Absent, orderDirection = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "skip" filledInOptionals.skip Encode.int, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "orderBy" filledInOptionals.orderBy (Encode.enum Contracts.UniSwapGraph.Enum.Burn_orderBy.toString), Argument.optional "orderDirection" filledInOptionals.orderDirection (Encode.enum Contracts.UniSwapGraph.Enum.OrderDirection.toString), Argument.optional "where" filledInOptionals.where_ Contracts.UniSwapGraph.InputObject.encodeBurn_filter ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "burns" optionalArgs object_ (identity >> Decode.nullable >> Decode.list)


type alias SwapsOptionalArguments =
    { skip : OptionalArgument Int
    , first : OptionalArgument Int
    , orderBy : OptionalArgument Contracts.UniSwapGraph.Enum.Swap_orderBy.Swap_orderBy
    , orderDirection : OptionalArgument Contracts.UniSwapGraph.Enum.OrderDirection.OrderDirection
    , where_ : OptionalArgument Contracts.UniSwapGraph.InputObject.Swap_filter
    }


swaps :
    (SwapsOptionalArguments -> SwapsOptionalArguments)
    -> SelectionSet decodesTo Contracts.UniSwapGraph.Object.Swap
    -> SelectionSet (List (Maybe decodesTo)) Contracts.UniSwapGraph.Object.Transaction
swaps fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { skip = Absent, first = Absent, orderBy = Absent, orderDirection = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "skip" filledInOptionals.skip Encode.int, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "orderBy" filledInOptionals.orderBy (Encode.enum Contracts.UniSwapGraph.Enum.Swap_orderBy.toString), Argument.optional "orderDirection" filledInOptionals.orderDirection (Encode.enum Contracts.UniSwapGraph.Enum.OrderDirection.toString), Argument.optional "where" filledInOptionals.where_ Contracts.UniSwapGraph.InputObject.encodeSwap_filter ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "swaps" optionalArgs object_ (identity >> Decode.nullable >> Decode.list)