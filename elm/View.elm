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
import Farm.View as Farm
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
import Stats.View as Stats
import Theme exposing (defaultTheme)
import Time
import TokenValue exposing (TokenValue)
import Tuple3
import Types exposing (..)
import UserNotice as UN exposing (UserNotice)
import UserTx
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
            model.trackedTxs
            model.trackedTxsExpanded
            (Wallet.userInfo model.wallet)
            model.showAddressId
        , case model.submodel of
            BlankInitialSubmodel ->
                Element.none

            Home homeModel ->
                Element.map HomeMsg <|
                    Home.view
                        model.dProfile
                        (Wallet.userInfo model.wallet)
                        homeModel

            Sentiment sentimentModel ->
                Element.map SentimentMsg <|
                    Sentiment.view
                        model.dProfile
                        (Wallet.userInfo model.wallet)
                        sentimentModel

            Stats statsModel ->
                Element.map StatsMsg <|
                    Stats.view
                        model.dProfile
                        (Wallet.userInfo model.wallet)
                        statsModel

            Farm farmModel ->
                Element.map FarmMsg <|
                    Farm.view
                        model.dProfile
                        farmModel
        ]


header : EH.DisplayProfile -> UserTx.Tracker Msg -> Bool -> Maybe UserInfo -> Maybe PhaceIconId -> Element Msg
header dProfile trackedTxs trackedTxsExpanded maybeUserInfo showAddressId =
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
        , (Maybe.map
            (Element.el
                [ Element.centerY
                , Element.alignRight
                ]
            )
           <|
            maybeTxTracker dProfile trackedTxsExpanded trackedTxs
          )
            |> Maybe.withDefault Element.none
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


maybeTxTracker : DisplayProfile -> Bool -> UserTx.Tracker Msg -> Maybe (Element Msg)
maybeTxTracker dProfile showExpanded trackedTxs =
    if List.isEmpty trackedTxs then
        Nothing

    else
        let
            tallyFunc : UserTx.TrackedTx Msg -> ( Int, Int, Int ) -> ( Int, Int, Int )
            tallyFunc trackedTx totals =
                case trackedTx.status of
                    UserTx.Signed _ UserTx.Mining ->
                        Tuple3.mapFirst ((+) 1) totals

                    UserTx.Signed _ (UserTx.Success _) ->
                        Tuple3.mapSecond ((+) 1) totals

                    UserTx.Signed _ UserTx.Failed ->
                        Tuple3.mapThird ((+) 1) totals

                    _ ->
                        totals

            tallies =
                trackedTxs
                    |> List.foldl tallyFunc ( 0, 0, 0 )

            renderedTallyEls =
                tallies
                    |> TupleHelpers.mapTuple3
                        (\n ->
                            if n == 0 then
                                Nothing

                            else
                                Just n
                        )
                    |> TupleHelpers.mapEachTuple3
                        (Maybe.map
                            (\n ->
                                Element.el
                                    [ Element.Font.color <| trackedTxMiningColor ]
                                <|
                                    Element.text <|
                                        String.fromInt n
                                            ++ " TXs mining"
                            )
                        )
                        (Maybe.map
                            (\n ->
                                Element.el
                                    [ Element.Font.color <| trackedTxSuccessColor ]
                                <|
                                    Element.text <|
                                        String.fromInt n
                                            ++ " TXs mined"
                            )
                        )
                        (Maybe.map
                            (\n ->
                                Element.el
                                    [ Element.Font.color <| trackedTxFailedColor ]
                                <|
                                    Element.text <|
                                        String.fromInt n
                                            ++ " TXs failed"
                            )
                        )
                    |> TupleHelpers.tuple3ToList
        in
        if List.all Maybe.Extra.isNothing renderedTallyEls then
            Nothing

        else
            Just <|
                Element.el
                    [ Element.below <|
                        if showExpanded then
                            Element.el
                                [ Element.alignRight
                                , Element.alignTop
                                ]
                            <|
                                trackedTxsColumn trackedTxs

                        else
                            Element.none
                    ]
                <|
                    Element.column
                        [ Element.Border.rounded 5
                        , Element.Background.color <| Element.rgb 0.2 0.2 0.2
                        , Element.padding (10 |> changeForMobile 5 dProfile)
                        , Element.spacing (10 |> changeForMobile 5 dProfile)
                        , Element.Font.size (20 |> changeForMobile 12 dProfile)
                        , Element.pointer
                        , EH.onClickNoPropagation <|
                            if showExpanded then
                                ShowExpandedTrackedTxs False

                            else
                                ShowExpandedTrackedTxs True
                        ]
                        (renderedTallyEls
                            |> List.map (Maybe.withDefault Element.none)
                        )


trackedTxsColumn : UserTx.Tracker Msg -> Element Msg
trackedTxsColumn trackedTxs =
    Element.column
        [ Element.Background.color <| Theme.lightBlue
        , Element.Border.rounded 3
        , Element.Border.glow
            (Element.rgba 0 0 0 0.2)
            4
        , Element.padding 10
        , Element.spacing 5
        , EH.onClickNoPropagation Types.NoOp
        , Element.height (Element.shrink |> Element.maximum 400)
        , Element.scrollbarY
        , Element.alignRight
        ]
        (trackedTxs
            |> List.indexedMap
                (\trackedTxId trackedTx ->
                    case trackedTx.status of
                        UserTx.Signed txHash signedTxStatus ->
                            Just <|
                                viewTrackedTxRow trackedTxId trackedTx.txInfo txHash signedTxStatus

                        _ ->
                            Nothing
                )
            |> Maybe.Extra.values
        )


viewTrackedTxRow : Int -> UserTx.TxInfo -> TxHash -> UserTx.SignedTxStatus -> Element Msg
viewTrackedTxRow trackedTxId txInfo txHash signedTxStatus =
    let
        etherscanLink label =
            Element.newTabLink
                [ Element.Font.italic
                , Element.Font.color defaultTheme.linkTextColor
                ]
                { url = EthHelpers.etherscanTxUrl txHash
                , label = Element.text label
                }

        titleEl =
            Element.text <|
                case txInfo of
                    UserTx.StakingApprove ->
                        "Enable Farming Deposit"

                    UserTx.StakingDeposit amount ->
                        "Deposit "
                            ++ TokenValue.toConciseString amount
                            ++ " ETHFRY"

                    UserTx.StakingWithdraw amount ->
                        "Withrdaw "
                            ++ TokenValue.toConciseString amount
                            ++ " ETHFRY"

                    UserTx.StakingClaim ->
                        "Claim FRY Rewards"

                    UserTx.StakingExit ->
                        "Exit Farming"

        -- case ( trackedTx.txInfo, trackedTx.status ) of
        --     ( UnlockTx, _ ) ->
        --         Element.text "Unlock DAI"
        --     ( TipTx postId amount, _ ) ->
        --         Element.row
        --             []
        --             [ Element.text "Tip "
        --             , Element.el
        --                 [ Element.Font.color defaultTheme.linkTextColor
        --                 , Element.pointer
        --                 , Element.Events.onClick <|
        --                     MsgUp <|
        --                         GotoRoute <|
        --                             Routing.ViewContext <|
        --                                 Post.ForPost postId
        --                 ]
        --                 (Element.text "Post")
        --             ]
        --     ( BurnTx postId amount, _ ) ->
        --         Element.row
        --             []
        --             [ Element.text "Burn for "
        --             , Element.el
        --                 [ Element.Font.color defaultTheme.linkTextColor
        --                 , Element.pointer
        --                 , Element.Events.onClick <|
        --                     MsgUp <|
        --                         GotoRoute <|
        --                             Routing.ViewContext <|
        --                                 Post.ForPost postId
        --                 ]
        --                 (Element.text "Post")
        --             ]
        --     ( PostTx _, Mined _ ) ->
        --         Element.text "Post"
        --     ( PostTx draft, _ ) ->
        --         Element.row
        --             [ Element.spacing 8
        --             ]
        --             [ Element.text "Post"
        --             , Element.el
        --                 [ Element.Font.color defaultTheme.linkTextColor
        --                 , Element.pointer
        --                 , Element.Events.onClick <| ViewDraft <| Just draft
        --                 ]
        --                 (Element.text "(View Draft)")
        --             ]
        statusEl =
            case signedTxStatus of
                UserTx.Mining ->
                    etherscanLink "Mining"

                UserTx.Failed ->
                    etherscanLink "Failed"

                UserTx.Success txReceipt ->
                    etherscanLink "Mined"
    in
    Element.row
        [ Element.width <| Element.px 300
        , Element.Background.color
            (signedTxStatusToColor signedTxStatus
                |> EH.withAlpha 0.3
            )
        , Element.Border.rounded 2
        , Element.Border.width 1
        , Element.Border.color <| Element.rgba 0 0 0 0.3
        , Element.padding 4
        , Element.spacing 4
        , Element.Font.size 20
        ]
        [ titleEl
        , Element.el [ Element.alignRight ] <| statusEl
        ]


trackedTxMiningColor =
    Theme.darkYellow


trackedTxFailedColor =
    Theme.softRed


trackedTxSuccessColor =
    Theme.green


signedTxStatusToColor : UserTx.SignedTxStatus -> Element.Color
signedTxStatusToColor signedStatus =
    case signedStatus of
        UserTx.Mining ->
            Theme.darkYellow

        UserTx.Success _ ->
            Theme.green

        UserTx.Failed ->
            Theme.softRed
