-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Contracts.UniSwapGraph.Enum.Transaction_orderBy exposing (..)

import Json.Decode as Decode exposing (Decoder)


type Transaction_orderBy
    = Id
    | BlockNumber
    | Timestamp
    | Mints
    | Burns
    | Swaps


list : List Transaction_orderBy
list =
    [ Id, BlockNumber, Timestamp, Mints, Burns, Swaps ]


decoder : Decoder Transaction_orderBy
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "id" ->
                        Decode.succeed Id

                    "blockNumber" ->
                        Decode.succeed BlockNumber

                    "timestamp" ->
                        Decode.succeed Timestamp

                    "mints" ->
                        Decode.succeed Mints

                    "burns" ->
                        Decode.succeed Burns

                    "swaps" ->
                        Decode.succeed Swaps

                    _ ->
                        Decode.fail ("Invalid Transaction_orderBy type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : Transaction_orderBy -> String
toString enum =
    case enum of
        Id ->
            "id"

        BlockNumber ->
            "blockNumber"

        Timestamp ->
            "timestamp"

        Mints ->
            "mints"

        Burns ->
            "burns"

        Swaps ->
            "swaps"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe Transaction_orderBy
fromString enumString =
    case enumString of
        "id" ->
            Just Id

        "blockNumber" ->
            Just BlockNumber

        "timestamp" ->
            Just Timestamp

        "mints" ->
            Just Mints

        "burns" ->
            Just Burns

        "swaps" ->
            Just Swaps

        _ ->
            Nothing