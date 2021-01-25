module Routing exposing (..)

import Eth.Types exposing (Address, Hex)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Query


type Route
    = Home
    | Sentiment
    | Stats
    | Farm
    | DerivedEth
    | NotFound String


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Sentiment (Parser.s "sentiment")
        , Parser.map Stats (Parser.s "stats")
        , Parser.map Farm (Parser.s "farm")
        , Parser.map DerivedEth (Parser.s "deth")
        ]


routeToString : String -> Route -> String
routeToString basePath route =
    basePath
        ++ (case route of
                Home ->
                    Builder.relative
                        [ "#" ]
                        []

                Sentiment ->
                    Builder.relative
                        [ "#", "sentiment" ]
                        []

                Stats ->
                    Builder.relative
                        [ "#", "stats" ]
                        []

                Farm ->
                    Builder.relative
                        [ "#", "farm" ]
                        []

                DerivedEth ->
                    Builder.relative
                        [ "#", "deth" ]
                        []

                NotFound _ ->
                    Builder.relative
                        [ "#" ]
                        []
           )


urlToRoute : Url -> Route
urlToRoute url =
    Maybe.withDefault (NotFound "url not found") (Parser.parse routeParser url)
