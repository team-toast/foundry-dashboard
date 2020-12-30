-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Contracts.UniSwapGraph.Enum.Bundle_orderBy exposing (..)

import Json.Decode as Decode exposing (Decoder)


type Bundle_orderBy
    = Id
    | EthPrice


list : List Bundle_orderBy
list =
    [ Id, EthPrice ]


decoder : Decoder Bundle_orderBy
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "id" ->
                        Decode.succeed Id

                    "ethPrice" ->
                        Decode.succeed EthPrice

                    _ ->
                        Decode.fail ("Invalid Bundle_orderBy type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : Bundle_orderBy -> String
toString enum =
    case enum of
        Id ->
            "id"

        EthPrice ->
            "ethPrice"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe Bundle_orderBy
fromString enumString =
    case enumString of
        "id" ->
            Just Id

        "ethPrice" ->
            Just EthPrice

        _ ->
            Nothing
