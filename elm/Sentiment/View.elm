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
import Helpers.Element as EH exposing (DisplayProfile(..), changeForMobile, responsiveVal)
import Helpers.Tuple as TupleHelpers
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
                    , viewPolls dProfile maybeUserInfo polls model.validatedResponses
                    ]


titleText : DisplayProfile -> String -> Element Msg
titleText dProfile title =
    Element.el
        [ Element.Font.bold
        , Element.Font.size <| responsiveVal dProfile 50 36
        ]
    <|
        Element.text title


viewPolls : DisplayProfile -> Maybe UserInfo -> List Poll -> ValidatedResponseTracker -> Element Msg
viewPolls dProfile maybeUserInfo polls validatedResponses =
    Element.column
        [ Element.spacing 20 ]
        (List.map
            (viewPoll dProfile maybeUserInfo validatedResponses)
            (List.reverse polls)
        )


viewPoll : DisplayProfile -> Maybe UserInfo -> ValidatedResponseTracker -> Poll -> Element Msg
viewPoll dProfile maybeUserInfo validatedResponses poll =
    Element.column
        [ Element.spacing 10]
        [ Element.paragraph
            [ Element.Font.size <| responsiveVal dProfile 22 18 ]
            [ Element.text poll.question ]
        , Element.el
            [ Element.padding 10
            , Element.width Element.fill
            ]
            (viewOptions dProfile maybeUserInfo poll validatedResponses)
        ]


viewOptions : DisplayProfile -> Maybe UserInfo -> Poll -> ValidatedResponseTracker -> Element Msg
viewOptions dProfile maybeUserInfo poll validatedResponses =
    Element.column
        [ Element.spacing 10
        , Element.width Element.fill
        ]
        (List.map (viewOption dProfile maybeUserInfo poll validatedResponses) poll.options)


viewOption : DisplayProfile -> Maybe UserInfo -> Poll -> ValidatedResponseTracker -> PollOption -> Element Msg
viewOption dProfile maybeUserInfo poll validatedResponses pollOption =
    let
        onClickMsg =
            case maybeUserInfo of
                Just userInfo ->
                    OptionClicked userInfo poll pollOption.id

                Nothing ->
                    MsgUp ConnectToWeb3

        tally =
            validatedResponses
                |> Dict.get poll.id
                |> Maybe.withDefault Dict.empty
                |> Dict.toList
                |> List.filter
                    (\( _, validatedResponse ) ->
                        validatedResponse.pollOptionId == pollOption.id
                    )
                |> List.length
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
            (Element.text <| String.fromInt tally)
        ]
