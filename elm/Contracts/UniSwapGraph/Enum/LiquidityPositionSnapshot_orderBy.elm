-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Contracts.UniSwapGraph.Enum.LiquidityPositionSnapshot_orderBy exposing (..)

import Json.Decode as Decode exposing (Decoder)


type LiquidityPositionSnapshot_orderBy
    = Id
    | LiquidityPosition
    | Timestamp
    | Block
    | User
    | Pair
    | Token0PriceUSD
    | Token1PriceUSD
    | Reserve0
    | Reserve1
    | ReserveUSD
    | LiquidityTokenTotalSupply
    | LiquidityTokenBalance


list : List LiquidityPositionSnapshot_orderBy
list =
    [ Id, LiquidityPosition, Timestamp, Block, User, Pair, Token0PriceUSD, Token1PriceUSD, Reserve0, Reserve1, ReserveUSD, LiquidityTokenTotalSupply, LiquidityTokenBalance ]


decoder : Decoder LiquidityPositionSnapshot_orderBy
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "id" ->
                        Decode.succeed Id

                    "liquidityPosition" ->
                        Decode.succeed LiquidityPosition

                    "timestamp" ->
                        Decode.succeed Timestamp

                    "block" ->
                        Decode.succeed Block

                    "user" ->
                        Decode.succeed User

                    "pair" ->
                        Decode.succeed Pair

                    "token0PriceUSD" ->
                        Decode.succeed Token0PriceUSD

                    "token1PriceUSD" ->
                        Decode.succeed Token1PriceUSD

                    "reserve0" ->
                        Decode.succeed Reserve0

                    "reserve1" ->
                        Decode.succeed Reserve1

                    "reserveUSD" ->
                        Decode.succeed ReserveUSD

                    "liquidityTokenTotalSupply" ->
                        Decode.succeed LiquidityTokenTotalSupply

                    "liquidityTokenBalance" ->
                        Decode.succeed LiquidityTokenBalance

                    _ ->
                        Decode.fail ("Invalid LiquidityPositionSnapshot_orderBy type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : LiquidityPositionSnapshot_orderBy -> String
toString enum =
    case enum of
        Id ->
            "id"

        LiquidityPosition ->
            "liquidityPosition"

        Timestamp ->
            "timestamp"

        Block ->
            "block"

        User ->
            "user"

        Pair ->
            "pair"

        Token0PriceUSD ->
            "token0PriceUSD"

        Token1PriceUSD ->
            "token1PriceUSD"

        Reserve0 ->
            "reserve0"

        Reserve1 ->
            "reserve1"

        ReserveUSD ->
            "reserveUSD"

        LiquidityTokenTotalSupply ->
            "liquidityTokenTotalSupply"

        LiquidityTokenBalance ->
            "liquidityTokenBalance"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe LiquidityPositionSnapshot_orderBy
fromString enumString =
    case enumString of
        "id" ->
            Just Id

        "liquidityPosition" ->
            Just LiquidityPosition

        "timestamp" ->
            Just Timestamp

        "block" ->
            Just Block

        "user" ->
            Just User

        "pair" ->
            Just Pair

        "token0PriceUSD" ->
            Just Token0PriceUSD

        "token1PriceUSD" ->
            Just Token1PriceUSD

        "reserve0" ->
            Just Reserve0

        "reserve1" ->
            Just Reserve1

        "reserveUSD" ->
            Just ReserveUSD

        "liquidityTokenTotalSupply" ->
            Just LiquidityTokenTotalSupply

        "liquidityTokenBalance" ->
            Just LiquidityTokenBalance

        _ ->
            Nothing