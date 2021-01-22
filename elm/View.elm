module View exposing (root)

import Browser
import Common.Msg exposing (..)
import Common.Types exposing (..)
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
import Helpers.Element as EH exposing (DisplayProfile(..), responsiveVal)
import Helpers.Eth as EthHelpers
import Helpers.List as ListHelpers
import Helpers.Time as TimeHelpers
import Helpers.Tuple as TupleHelpers
import Html.Attributes
import Images
import Json.Decode
import List.Extra
import Maybe.Extra
import MaybeDebugLog exposing (maybeDebugLog)
import Phace
import Routing exposing (Route)
import Theme exposing (defaultTheme)
import Time
import TokenValue exposing (TokenValue)
import Tuple3
import Types exposing (..)
import UserNotice as UN exposing (UserNotice)
import UserTx
import View.Common exposing (..)
import View.DerivedEth as DerivedEth
import View.Farm as Farm
import View.Home as Home
import View.Sentiment as Sentiment
import View.Stats as Stats
import Wallet exposing (Wallet)


root :
    Model
    -> Browser.Document Msg
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


modals :
    Model
    -> List (Element Msg)
modals model =
    Maybe.Extra.values
        ([ if not model.cookieConsentGranted then
            Just <| viewCookieConsentModal model.dProfile

           else
            Nothing
         ]
            ++ List.map Just
                (userNoticeEls
                    model.dProfile
                    model.userNotices
                )
        )


viewCookieConsentModal :
    DisplayProfile
    -> Element Msg
viewCookieConsentModal dProfile =
    Element.row
        [ Element.alignBottom
        , responsiveVal dProfile Element.centerX (Element.width Element.fill)
        , Element.Border.roundEach
            { topLeft = 5
            , topRight = 5
            , bottomLeft = 0
            , bottomRight = 0
            }
        , Element.padding 15
        , Element.spacing 15
        , Element.Background.color <| Theme.darkBlue
        , Element.Font.color EH.white
        , Element.Border.glow
            (Element.rgba 0 0 0 0.2)
            10
        ]
        [ Element.paragraph
            [ Element.width <| responsiveVal dProfile (Element.px 800) Element.fill
            , Element.Font.size <| responsiveVal dProfile 20 12
            ]
            [ Element.text "Foundry products use cookies and analytics to track behavior patterns, to help zero in on effective marketing strategies. To avoid being tracked in this way, we recommend using the "
            , Element.newTabLink
                [ Element.Font.color Theme.blue ]
                { url = "https://brave.com/"
                , label = Element.text "Brave browser"
                }
            , Element.text " or installing the "
            , Element.newTabLink
                [ Element.Font.color Theme.blue ]
                { url = "https://tools.google.com/dlpage/gaoptout"
                , label = Element.text "Google Analytics Opt-Out browser addon"
                }
            , Element.text "."
            ]
        , Theme.blueButton dProfile [] [ "Understood" ] CookieConsentGranted
        ]


body :
    Model
    -> Element Msg
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
                Home.view
                    model.dProfile
                    (Wallet.userInfo model.wallet)
                    homeModel
                    |> Element.map HomeMsg

            Sentiment sentimentModel ->
                Sentiment.view
                    model.dProfile
                    (Wallet.userInfo model.wallet)
                    sentimentModel
                    |> Element.map SentimentMsg

            Stats statsModel ->
                Stats.view
                    model.dProfile
                    (Wallet.userInfo model.wallet)
                    statsModel
                    |> Element.map StatsMsg

            DerivedEth derivedEthModel ->
                DerivedEth.view
                    model.dProfile
                    (Wallet.userInfo model.wallet)
                    derivedEthModel
                    |> Element.map DerivedEthMsg

            Farm farmModel ->
                Farm.view
                    model.dProfile
                    farmModel
                    |> Element.map FarmMsg
        ]


header :
    DisplayProfile
    -> UserTx.Tracker Msg
    -> Bool
    -> Maybe UserInfo
    -> Maybe PhaceIconId
    -> Element Msg
header dProfile trackedTxs trackedTxsExpanded maybeUserInfo showAddressId =
    Element.row
        [ Element.width Element.fill
        , Element.Background.color defaultTheme.headerBackground
        , Element.padding <|
            responsiveVal
                dProfile
                20
                10
        , Element.spacing <|
            responsiveVal
                dProfile
                10
                5
        , Element.Border.glow
            (EH.black |> EH.withAlpha 0.5)
            5
        ]
        (case dProfile of
            Desktop ->
                [ logoBlock dProfile
                , (Maybe.map
                    (Element.el
                        [ Element.alignTop
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

            Mobile ->
                [ Element.column
                    [ Element.alignTop
                    , Element.alignLeft
                    ]
                    [ Element.el
                        [ Element.alignTop
                        , Element.alignLeft
                        ]
                      <|
                        logoBlock dProfile
                    , (Maybe.map
                        (Element.el
                            [ Element.alignTop
                            , Element.alignRight
                            ]
                        )
                       <|
                        maybeTxTracker dProfile trackedTxsExpanded trackedTxs
                      )
                        |> Maybe.withDefault Element.none
                    ]
                , Element.el
                    [ Element.centerY
                    , Element.alignRight
                    ]
                  <|
                    connectButtonOrPhace dProfile maybeUserInfo showAddressId
                ]
        )


logoBlock :
    DisplayProfile
    -> Element Msg
logoBlock dProfile =
    Element.row
        [ Element.height Element.fill
        , Element.padding <|
            responsiveVal
                dProfile
                10
                7
        , Element.spacing <|
            responsiveVal
                dProfile
                20
                10
        ]
        [ Images.toElement
            [ Element.centerY
            , Element.width <|
                Element.px <|
                    responsiveVal
                        dProfile
                        60
                        30
            ]
            Images.fryIcon
        , Element.column
            [ Element.spacing 5 ]
            [ Element.el
                [ Element.Font.color EH.white
                , Element.Font.size <|
                    responsiveVal
                        dProfile
                        35
                        20
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
                , Element.Font.size <|
                    responsiveVal
                        dProfile
                        18
                        10
                ]
                { url = "https://foundrydao.com"
                , label =
                    Element.text "What is Foundry?"
                }
            ]
        ]


connectButtonOrPhace :
    DisplayProfile
    -> Maybe UserInfo
    -> Maybe PhaceIconId
    -> Element Msg
connectButtonOrPhace dProfile maybeUserInfo showAddressInfo =
    case maybeUserInfo of
        Nothing ->
            web3ConnectButton
                dProfile
                ([]
                    ++ (case dProfile of
                            Desktop ->
                                []

                            Mobile ->
                                [ Element.Font.size 10
                                , Element.padding 5
                                ]
                       )
                )
                MsgUp

        Just userInfo ->
            phaceElement
                False
                userInfo.address
                (showAddressInfo == Just UserPhace)
                dProfile
                (MsgUp <| ShowOrHideAddress UserPhace)
                Types.NoOp


userNoticeEls :
    DisplayProfile
    -> List UserNotice
    -> List (Element Msg)
userNoticeEls dProfile notices =
    if notices == [] then
        []

    else
        [ Element.column
            [ Element.moveLeft <|
                EH.responsiveVal
                    dProfile
                    20
                    5
            , Element.moveUp <|
                EH.responsiveVal
                    dProfile
                    20
                    5
            , Element.spacing <|
                EH.responsiveVal
                    dProfile
                    10
                    5
            , Element.alignRight
            , Element.alignBottom
            , Element.width <|
                Element.px <|
                    EH.responsiveVal
                        dProfile
                        300
                        150
            , Element.Font.size <|
                EH.responsiveVal
                    dProfile
                    15
                    10
            ]
            (notices
                |> List.indexedMap (\id notice -> ( id, notice ))
                |> List.filter (\( _, notice ) -> notice.align == UN.BottomRight)
                |> List.map (userNotice dProfile)
            )
        , Element.column
            [ Element.moveRight <|
                EH.responsiveVal
                    dProfile
                    20
                    5
            , Element.moveDown 100
            , Element.spacing <|
                EH.responsiveVal
                    dProfile
                    10
                    5
            , Element.alignLeft
            , Element.alignTop
            , Element.width <|
                Element.px <|
                    EH.responsiveVal
                        dProfile
                        300
                        150
            , Element.Font.size <|
                EH.responsiveVal
                    dProfile
                    15
                    10
            ]
            (notices
                |> List.indexedMap (\id notice -> ( id, notice ))
                |> List.filter (\( _, notice ) -> notice.align == UN.TopLeft)
                |> List.map (userNotice dProfile)
            )
        ]


userNotice :
    DisplayProfile
    -> ( Int, UserNotice )
    -> Element Msg
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
        , Element.Border.rounded <|
            EH.responsiveVal
                dProfile
                10
                5
        , Element.padding <|
            EH.responsiveVal
                dProfile
                8
                3
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


maybeTxTracker :
    DisplayProfile
    -> Bool
    -> UserTx.Tracker Msg
    -> Maybe (Element Msg)
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
                        , Element.Border.width 2
                        , Element.Border.color Theme.blue
                        , Element.Background.color <| Element.rgb 0.2 0.2 0.2
                        , Element.padding <|
                            responsiveVal
                                dProfile
                                10
                                5
                        , Element.spacing <|
                            responsiveVal
                                dProfile
                                10
                                5
                        , Element.Font.size <|
                            responsiveVal
                                dProfile
                                20
                                12
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


trackedTxsColumn :
    UserTx.Tracker Msg
    -> Element Msg
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
                                viewTrackedTxRow
                                    trackedTxId
                                    trackedTx.txInfo
                                    txHash
                                    signedTxStatus

                        _ ->
                            Nothing
                )
            |> Maybe.Extra.values
        )


viewTrackedTxRow :
    Int
    -> UserTx.TxInfo
    -> TxHash
    -> UserTx.SignedTxStatus
    -> Element Msg
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
                        "Withdraw "
                            ++ TokenValue.toConciseString amount
                            ++ " ETHFRY"

                    UserTx.StakingClaim ->
                        "Claim FRY Rewards"

                    UserTx.StakingExit ->
                        "Exit Farming"

                    UserTx.DEthRedeem ->
                        "Redeem dETH"

                    UserTx.DEthDeposit ->
                        "Squander ETH"

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


signedTxStatusToColor :
    UserTx.SignedTxStatus
    -> Element.Color
signedTxStatusToColor signedStatus =
    case signedStatus of
        UserTx.Mining ->
            trackedTxMiningColor

        UserTx.Success _ ->
            trackedTxSuccessColor

        UserTx.Failed ->
            trackedTxFailedColor


trackedTxMiningColor : Element.Color
trackedTxMiningColor =
    Theme.darkYellow


trackedTxFailedColor : Element.Color
trackedTxFailedColor =
    Theme.softRed


trackedTxSuccessColor : Element.Color
trackedTxSuccessColor =
    Theme.green
