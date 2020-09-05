module Sentiment.View exposing (view)

import Common.Msg exposing (..)
import Common.Types exposing (..)
import Common.View exposing (..)
import Config
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Eth.Utils
import FormatFloat
import Helpers.Element as EH exposing (DisplayProfile(..), changeForMobile, responsiveVal)
import Helpers.Tuple as TupleHelpers
import Maybe.Extra
import Routing exposing (Route)
import Sentiment.Types exposing (..)
import Theme exposing (darkTheme, defaultTheme)
import TokenValue exposing (TokenValue)
import Wallet exposing (Wallet)


view : DisplayProfile -> Maybe UserInfo -> Model -> Element Msg
view dProfile maybeUserInfo model =
    Element.el
        [ responsiveVal dProfile
            (Element.paddingXY 100 50)
            (Element.paddingXY 20 20)
        , Element.width Element.fill
        , Element.Font.color EH.white
        ]
    <|
        case model.polls of
            Nothing ->
                titleText dProfile "Loading polls..."

            Just polls ->
                Element.column
                    [ Element.width Element.fill
                    , Element.spacing 50
                    ]
                    [ titleText dProfile "Foundry Polls"
                    , viewPolls dProfile maybeUserInfo polls model.validatedResponses model.fryBalances
                    ]


titleText : DisplayProfile -> String -> Element Msg
titleText dProfile title =
    Element.el
        [ Element.Font.bold
        , Element.Font.size <| responsiveVal dProfile 50 36
        ]
    <|
        Element.text title


viewPolls : DisplayProfile -> Maybe UserInfo -> List Poll -> ValidatedResponseTracker -> Dict String (Maybe TokenValue) -> Element Msg
viewPolls dProfile maybeUserInfo polls validatedResponses fryBalances =
    Element.column
        [ Element.spacing 20 ]
        (List.map
            (viewPoll dProfile maybeUserInfo validatedResponses fryBalances)
            (List.reverse polls)
        )


viewPoll : DisplayProfile -> Maybe UserInfo -> ValidatedResponseTracker -> Dict String (Maybe TokenValue) -> Poll -> Element Msg
viewPoll dProfile maybeUserInfo validatedResponses fryBalances poll =
    let
        validatedResponsesForPoll =
            validatedResponses
                |> Dict.get poll.id
                |> Maybe.withDefault Dict.empty

        foldFunc : ( String, ValidatedResponse ) -> ( Dict Int TokenValue, TokenValue ) -> ( Dict Int TokenValue, TokenValue )
        foldFunc ( addressString, validatedResponse ) ( accTallies, accTotal ) =
            let
                fryAmount =
                    fryBalances
                        |> Dict.get (addressString |> String.toLower)
                        |> Maybe.Extra.join
                        |> Maybe.withDefault TokenValue.zero
            in
            ( accTallies
                |> Dict.update validatedResponse.pollOptionId
                    (Maybe.map (TokenValue.add fryAmount))
            , accTotal
                |> TokenValue.add fryAmount
            )

        initTallyDict =
            poll.options
                |> List.map
                    (\option ->
                        ( option.id, TokenValue.zero )
                    )
                |> Dict.fromList

        ( talliedFryForOptions, totalFryVoted ) =
            validatedResponsesForPoll
                |> Dict.toList
                |> List.foldl
                    foldFunc
                    ( initTallyDict
                    , TokenValue.zero
                    )
    in
    Element.column
        [ Element.spacing 10 ]
        [ Element.paragraph
            [ Element.Font.size <| responsiveVal dProfile 22 18 ]
            [ Element.text poll.question ]
        , Element.el
            [ Element.padding 10
            , Element.width Element.fill
            ]
            (viewOptions dProfile maybeUserInfo poll talliedFryForOptions totalFryVoted)
        ]


viewOptions : DisplayProfile -> Maybe UserInfo -> Poll -> Dict Int TokenValue -> TokenValue -> Element Msg
viewOptions dProfile maybeUserInfo poll talliedFryForOptions totalFryVoted =
    Element.column
        [ Element.spacing 10
        , Element.width Element.fill
        ]
        (poll.options
            |> List.map
                (\option ->
                    let
                        supportFloat =
                            TokenValue.getRatioWithWarning
                                (talliedFryForOptions
                                    |> Dict.get option.id
                                    |> Maybe.withDefault TokenValue.zero
                                )
                                totalFryVoted
                    in
                    viewOption dProfile maybeUserInfo poll supportFloat option
                )
        )


viewOption : DisplayProfile -> Maybe UserInfo -> Poll -> Float -> PollOption -> Element Msg
viewOption dProfile maybeUserInfo poll supportFloat pollOption =
    let
        onClickMsg =
            case maybeUserInfo of
                Just userInfo ->
                    OptionClicked userInfo poll pollOption.id

                Nothing ->
                    MsgUp ConnectToWeb3
    in
    Element.row
        [ Element.width Element.fill ]
        [ Element.paragraph
            [ Element.Font.size <| responsiveVal dProfile 18 14
            , Element.Events.onClick onClickMsg
            , Element.pointer
            ]
            [ Element.text pollOption.name ]
        , Element.el
            [ Element.alignRight ]
            (Element.text <|
                (FormatFloat.formatFloat 1 (supportFloat * 100)
                    ++ "%"
                )
            )
        ]
