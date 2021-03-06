-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Contracts.UniSwapGraph.Enum.Mint_orderBy exposing (..)

import Json.Decode as Decode exposing (Decoder)


type Mint_orderBy
    = Id
    | Transaction
    | Timestamp
    | Pair
    | To
    | Liquidity
    | Sender
    | Amount0
    | Amount1
    | LogIndex
    | AmountUSD
    | FeeTo
    | FeeLiquidity


list : List Mint_orderBy
list =
    [ Id, Transaction, Timestamp, Pair, To, Liquidity, Sender, Amount0, Amount1, LogIndex, AmountUSD, FeeTo, FeeLiquidity ]


decoder : Decoder Mint_orderBy
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "id" ->
                        Decode.succeed Id

                    "transaction" ->
                        Decode.succeed Transaction

                    "timestamp" ->
                        Decode.succeed Timestamp

                    "pair" ->
                        Decode.succeed Pair

                    "to" ->
                        Decode.succeed To

                    "liquidity" ->
                        Decode.succeed Liquidity

                    "sender" ->
                        Decode.succeed Sender

                    "amount0" ->
                        Decode.succeed Amount0

                    "amount1" ->
                        Decode.succeed Amount1

                    "logIndex" ->
                        Decode.succeed LogIndex

                    "amountUSD" ->
                        Decode.succeed AmountUSD

                    "feeTo" ->
                        Decode.succeed FeeTo

                    "feeLiquidity" ->
                        Decode.succeed FeeLiquidity

                    _ ->
                        Decode.fail ("Invalid Mint_orderBy type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : Mint_orderBy -> String
toString enum =
    case enum of
        Id ->
            "id"

        Transaction ->
            "transaction"

        Timestamp ->
            "timestamp"

        Pair ->
            "pair"

        To ->
            "to"

        Liquidity ->
            "liquidity"

        Sender ->
            "sender"

        Amount0 ->
            "amount0"

        Amount1 ->
            "amount1"

        LogIndex ->
            "logIndex"

        AmountUSD ->
            "amountUSD"

        FeeTo ->
            "feeTo"

        FeeLiquidity ->
            "feeLiquidity"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe Mint_orderBy
fromString enumString =
    case enumString of
        "id" ->
            Just Id

        "transaction" ->
            Just Transaction

        "timestamp" ->
            Just Timestamp

        "pair" ->
            Just Pair

        "to" ->
            Just To

        "liquidity" ->
            Just Liquidity

        "sender" ->
            Just Sender

        "amount0" ->
            Just Amount0

        "amount1" ->
            Just Amount1

        "logIndex" ->
            Just LogIndex

        "amountUSD" ->
            Just AmountUSD

        "feeTo" ->
            Just FeeTo

        "feeLiquidity" ->
            Just FeeLiquidity

        _ ->
            Nothing
