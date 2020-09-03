module Sentiment.View exposing (view)

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


view : DisplayProfile -> Model -> Maybe UserInfo -> Element Msg
view dProfile model maybeUserInfo =
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
                    , viewPolls dProfile polls
                    ]


titleText : DisplayProfile -> String -> Element Msg
titleText dProfile title =
    Element.el
        [ Element.Font.bold
        , Element.Font.size <| responsiveVal dProfile 50 36
        ]
    <|
        Element.text title


viewPolls : DisplayProfile -> List Poll -> Element Msg
viewPolls dProfile polls =
    Element.column
        [ Element.spacing 20 ]
        (List.map
            (viewPoll dProfile)
            (List.reverse polls)
        )


viewPoll : DisplayProfile -> Poll -> Element Msg
viewPoll dProfile poll =
    Element.column
        [ Element.spacing 10 ]
        [ Element.paragraph
            [ Element.Font.size <| responsiveVal dProfile 22 18 ]
            [ Element.text poll.question ]
        , Element.el
            [ Element.padding 10 ]
            (viewOptions dProfile poll.id poll.options)
        ]


viewOptions : DisplayProfile -> Int -> List PollOption -> Element Msg
viewOptions dProfile pollId options =
    Element.column
        [ Element.spacing 10 ]
        (List.map (viewOption dProfile pollId) options)


viewOption : DisplayProfile -> Int -> PollOption ->  Element Msg
viewOption dProfile pollId pollOption  =
    Element.paragraph
        [ Element.Font.size <| responsiveVal dProfile 18 14 
        , Element.Events.onClick <| OptionClicked pollId pollOption.id
        ]
        [ Element.text pollOption.name ]
