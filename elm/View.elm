module View exposing (root)

import Browser
import Common.Msg exposing (..)
import Common.Types exposing (..)
import Common.View exposing (..)
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Element.Lazy
import ElementMarkdown
import Eth.Types exposing (Address, Hex, TxHash)
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..), changeForMobile, responsiveVal)
import Helpers.Eth as EthHelpers
import Helpers.List as ListHelpers
import Helpers.Time as TimeHelpers
import Helpers.Tuple as TupleHelpers
import Home.View as Home
import Html.Attributes
import Images
import Json.Decode
import List.Extra
import Maybe.Extra
import MaybeDebugLog exposing (maybeDebugLog)
import Phace
import Routing exposing (Route)
import Sentiment.View as Sentiment
import Theme exposing (defaultTheme)
import Time
import TokenValue exposing (TokenValue)
import Tuple3
import Types exposing (..)
import UserNotice as UN exposing (UserNotice)
import Wallet exposing (Wallet)


root : Model -> Browser.Document Msg
root model =
    { title = "Dashboard - Foundry"
    , body =
        [ Element.layout
            ([ Element.width Element.fill
             , Element.htmlAttribute <| Html.Attributes.style "height" "100vh"
             , Element.Events.onClick ClickHappened
             , Element.Font.family
                [ Element.Font.typeface "DM Sans"
                , Element.Font.sansSerif
                ]
             ]
                ++ List.map Element.inFront (modals model)
            )
          <|
            body model
        ]
    }


modals : Model -> List (Element Msg)
modals model =
    Maybe.Extra.values
        ([]
            ++ List.map Just
                (userNoticeEls
                    model.dProfile
                    model.userNotices
                )
        )


body : Model -> Element Msg
body model =
    Element.column
        [ Element.width Element.fill
        , Element.Background.color defaultTheme.appBackground
        , Element.height Element.fill
        ]
        [ header
            model.dProfile
            model.submodel
            (Wallet.userInfo model.wallet)
            model.showAddressId
        , case model.submodel of
            Home homeModel ->
                Element.map HomeMsg <|
                    Home.view
                        model.dProfile
                        homeModel
                        (Wallet.userInfo model.wallet)

            Sentiment sentimentModel ->
                Element.map SentimentMsg <|
                    Sentiment.view
                        model.dProfile
                        sentimentModel
                        (Wallet.userInfo model.wallet)
        ]


header : EH.DisplayProfile -> Submodel -> Maybe UserInfo -> Maybe PhaceIconId -> Element Msg
header dProfile mode maybeUserInfo showAddressId =
    Element.row
        [ Element.width Element.fill
        , Element.Background.color defaultTheme.headerBackground
        , Element.padding (20 |> changeForMobile 10 dProfile)
        , Element.spacing (10 |> changeForMobile 5 dProfile)
        , Element.Border.glow
            (EH.black |> EH.withAlpha 0.5)
            5
        ]
        [ case dProfile of
            Mobile ->
                Element.el [ Element.alignTop, Element.alignLeft ] <| logoBlock dProfile

            Desktop ->
                logoBlock dProfile
        , Element.el
            [ Element.centerY
            , Element.alignRight
            ]
          <|
            connectButtonOrPhace dProfile maybeUserInfo showAddressId
        ]


logoBlock : EH.DisplayProfile -> Element Msg
logoBlock dProfile =
    Element.row
        [ Element.height Element.fill
        , Element.padding 10
        , Element.spacing 20
        ]
        [ Images.toElement
            [ Element.centerY
            , Element.width <| Element.px 60
            ]
            Images.fryIcon
        , Element.column
            [ Element.spacing 5 ]
            [ Element.el
                [ Element.Font.color EH.white
                , Element.Font.size 35
                , Element.Font.bold
                , Element.centerY
                ]
              <|
                Element.text "Foundry Dashboard"
            , Element.newTabLink
                [ Element.alignLeft
                , Element.Background.color Theme.blue
                , Element.paddingXY 10 3
                , Element.Border.rounded 4
                , Element.Font.color EH.white
                , Element.Font.size 18
                ]
                { url = "https://foundrydao.com"
                , label =
                    Element.text "What is Foundry?"
                }
            ]
        ]


connectButtonOrPhace : DisplayProfile -> Maybe UserInfo -> Maybe PhaceIconId -> Element Msg
connectButtonOrPhace dProfile maybeUserInfo showAddressInfo =
    case maybeUserInfo of
        Nothing ->
            web3ConnectButton dProfile [] MsgUp

        Just userInfo ->
            phaceElement
                False
                userInfo.address
                (showAddressInfo == Just UserPhace)
                (MsgUp <| ShowOrHideAddress UserPhace)
                Types.NoOp


userNoticeEls : EH.DisplayProfile -> List UserNotice -> List (Element Msg)
userNoticeEls dProfile notices =
    if notices == [] then
        []

    else
        [ Element.column
            [ Element.moveLeft (20 |> EH.changeForMobile 5 dProfile)
            , Element.moveUp (20 |> EH.changeForMobile 5 dProfile)
            , Element.spacing (10 |> EH.changeForMobile 5 dProfile)
            , Element.alignRight
            , Element.alignBottom
            , Element.width <| Element.px (300 |> EH.changeForMobile 150 dProfile)
            , Element.Font.size (15 |> EH.changeForMobile 10 dProfile)
            ]
            (notices
                |> List.indexedMap (\id notice -> ( id, notice ))
                |> List.filter (\( _, notice ) -> notice.align == UN.BottomRight)
                |> List.map (userNotice dProfile)
            )
        , Element.column
            [ Element.moveRight (20 |> EH.changeForMobile 5 dProfile)
            , Element.moveDown 100
            , Element.spacing (10 |> EH.changeForMobile 5 dProfile)
            , Element.alignLeft
            , Element.alignTop
            , Element.width <| Element.px (300 |> EH.changeForMobile 150 dProfile)
            , Element.Font.size (15 |> EH.changeForMobile 10 dProfile)
            ]
            (notices
                |> List.indexedMap (\id notice -> ( id, notice ))
                |> List.filter (\( _, notice ) -> notice.align == UN.TopLeft)
                |> List.map (userNotice dProfile)
            )
        ]


userNotice : EH.DisplayProfile -> ( Int, UserNotice ) -> Element Msg
userNotice dProfile ( id, notice ) =
    let
        color =
            case notice.noticeType of
                UN.Update ->
                    Element.rgb255 100 200 255

                UN.Caution ->
                    Element.rgb255 255 188 0

                UN.Error ->
                    Element.rgb255 255 70 70

                UN.ShouldBeImpossible ->
                    Element.rgb255 200 200 200

        textColor =
            case notice.noticeType of
                UN.Error ->
                    Element.rgb 1 1 1

                _ ->
                    Element.rgb 0 0 0

        closeElement =
            EH.closeButton
                [ Element.alignRight
                , Element.alignTop
                , Element.moveUp 2
                ]
                EH.black
                (DismissNotice id)
    in
    Element.el
        [ Element.Background.color color
        , Element.Border.rounded (10 |> EH.changeForMobile 5 dProfile)
        , Element.padding (8 |> EH.changeForMobile 3 dProfile)
        , Element.width Element.fill
        , Element.Border.width 1
        , Element.Border.color <| Element.rgba 0 0 0 0.15
        , EH.subtleShadow
        , EH.onClickNoPropagation Types.NoOp
        ]
        (notice.mainParagraphs
            |> List.map (List.map (Element.map never))
            |> List.indexedMap
                (\pNum paragraphLines ->
                    Element.paragraph
                        [ Element.width Element.fill
                        , Element.Font.color textColor
                        , Element.spacing 1
                        ]
                        (if pNum == 0 then
                            closeElement :: paragraphLines

                         else
                            paragraphLines
                        )
                )
            |> Element.column
                [ Element.spacing 4
                , Element.width Element.fill
                ]
        )
