-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Contracts.UniSwapGraph.Scalar exposing (BigDecimal(..), BigInt(..), Bytes(..), Codecs, Id(..), defaultCodecs, defineCodecs, unwrapCodecs, unwrapEncoder)

import Graphql.Codec exposing (Codec)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type BigDecimal
    = BigDecimal String


type BigInt
    = BigInt String


type Bytes
    = Bytes String


type Id
    = Id String


defineCodecs :
    { codecBigDecimal : Codec valueBigDecimal
    , codecBigInt : Codec valueBigInt
    , codecBytes : Codec valueBytes
    , codecId : Codec valueId
    }
    -> Codecs valueBigDecimal valueBigInt valueBytes valueId
defineCodecs definitions =
    Codecs definitions


unwrapCodecs :
    Codecs valueBigDecimal valueBigInt valueBytes valueId
    ->
        { codecBigDecimal : Codec valueBigDecimal
        , codecBigInt : Codec valueBigInt
        , codecBytes : Codec valueBytes
        , codecId : Codec valueId
        }
unwrapCodecs (Codecs unwrappedCodecs) =
    unwrappedCodecs


unwrapEncoder getter (Codecs unwrappedCodecs) =
    (unwrappedCodecs |> getter |> .encoder) >> Graphql.Internal.Encode.fromJson


type Codecs valueBigDecimal valueBigInt valueBytes valueId
    = Codecs (RawCodecs valueBigDecimal valueBigInt valueBytes valueId)


type alias RawCodecs valueBigDecimal valueBigInt valueBytes valueId =
    { codecBigDecimal : Codec valueBigDecimal
    , codecBigInt : Codec valueBigInt
    , codecBytes : Codec valueBytes
    , codecId : Codec valueId
    }


defaultCodecs : RawCodecs BigDecimal BigInt Bytes Id
defaultCodecs =
    { codecBigDecimal =
        { encoder = \(BigDecimal raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map BigDecimal
        }
    , codecBigInt =
        { encoder = \(BigInt raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map BigInt
        }
    , codecBytes =
        { encoder = \(Bytes raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map Bytes
        }
    , codecId =
        { encoder = \(Id raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map Id
        }
    }