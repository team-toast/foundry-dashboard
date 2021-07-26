module View exposing (view)

import BigInt
import Browser
import Browser.Navigation
import Config
import Element exposing (Element, alignBottom, alignRight, alignTop, below, centerX, column, el, fill, focusStyle, height, htmlAttribute, inFront, layoutWith, link, maximum, moveRight, newTabLink, none, padding, paddingXY, paragraph, rgba, row, scrollbarY, shrink, spacing, text, width)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import ElementHelpers as EH exposing (DisplayProfile(..), responsiveVal)
import Eth.Types exposing (TxHash)
import Eth.Utils
import Helpers.Eth as EthHelpers
import Helpers.Time as TimeHelpers
import Helpers.Tuple as TupleHelpers
import Html exposing (Html)
import Html.Attributes
import Images
import List exposing (map)
import Maybe.Extra
import Misc exposing (userInfo)
import Routing exposing (Route, routeName, routeToString)
import Theme exposing (defaultTheme)
import Time
import TokenValue
import Tuple3
import Types exposing (..)
import Update exposing (gotoRoute)
import UserNotice as UN exposing (UserNotice)
import UserTx
import View.Common exposing (..)
import View.Deth as DerivedEth
import View.Farm as Farm
import View.Home as Home
import View.Sentiment as Sentiment
import View.Stats as Stats


view : Model -> Browser.Document Msg
view model =
    { title = "Dashboard - Foundry"
    , body =
        viewPage model
            |> renderModals model
            |> List.singleton
    }


renderModals : Model -> Element Msg -> Html Msg
renderModals model =
    modals model
        |> map inFront
        |> (++)
            [ width fill
            , Html.Attributes.style "height" "100vh"
                |> htmlAttribute
            , Element.Events.onClick ClickHappened
            , Element.Font.family
                [ Element.Font.typeface "DM Sans"
                , Element.Font.sansSerif
                ]
            ]
        |> layoutWith
            { options =
                [ focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }


modals : Model -> List (Element Msg)
modals model =
    Maybe.Extra.values
        [ if model.cookieConsentGranted then
            Nothing

          else
            viewCookieConsentModal model.dProfile
                |> Just
        ]
        ++ userNoticeEls
            model.dProfile
            model.userNotices


viewCookieConsentModal : DisplayProfile -> Element Msg
viewCookieConsentModal dProfile =
    row
        [ alignBottom
        , responsiveVal dProfile centerX (width fill)
        , Element.Border.roundEach
            { topLeft = 5
            , topRight = 5
            , bottomLeft = 0
            , bottomRight = 0
            }
        , padding 15
        , spacing 15
        , Element.Background.color <| Theme.darkBlue
        , Element.Font.color EH.white
        , Element.Border.glow
            (rgba 0 0 0 0.2)
            10
        ]
        [ paragraph
            [ responsiveVal dProfile (Element.px 800) fill
                |> width
            , responsiveVal dProfile 20 12
                |> Element.Font.size
            ]
            [ text "Foundry products use cookies and analytics to track behavior patterns, to help zero in on effective marketing strategies. To avoid being tracked in this way, we recommend using the "
            , newTabLink
                [ Element.Font.color Theme.blue ]
                { url = "https://brave.com/"
                , label = text "Brave browser"
                }
            , text " or installing the "
            , newTabLink
                [ Element.Font.color Theme.blue ]
                { url = "https://tools.google.com/dlpage/gaoptout"
                , label = text "Google Analytics Opt-Out browser addon"
                }
            , text "."
            ]
        , Theme.blueButton dProfile
            []
            [ "Understood" ]
            (EH.Action CookieConsentGranted)
        ]


viewPage : Model -> Element Msg
viewPage model =
    column
        [ width fill
        , Element.Background.color defaultTheme.appBackground
        , height fill
        ]
        [ header model
        , viewBody model
        ]


viewBody : Model -> Element Msg
viewBody model =
    case model.route of
        Routing.Home ->
            Home.view model

        Routing.Farm ->
            Farm.view model

        Routing.Sentiment ->
            Sentiment.view model

        Routing.Deth ->
            DerivedEth.view model

        Routing.Stats ->
            Stats.view model

        _ ->
            Sentiment.view model


header :
    Model
    -> Element Msg
header model =
    let
        dProfile =
            model.dProfile
    in
    case dProfile of
        Desktop ->
            [ logoBlock dProfile
            , navigationButtons model
            , (Maybe.map
                (el
                    [ alignTop
                    , alignRight
                    ]
                )
               <|
                maybeTxTracker dProfile model.trackedTxsExpanded model.trackedTxs
              )
                |> Maybe.withDefault none
            , connectButtonOrPhace dProfile (userInfo model.wallet) model.showAddressId
                |> el
                    [ Element.centerY
                    , alignRight
                    ]
            ]
                |> row
                    [ fill
                        |> width
                    , Element.Background.color defaultTheme.headerBackground
                    , responsiveVal dProfile 20 10
                        |> padding
                    , responsiveVal dProfile 10 5
                        |> spacing
                    , Element.Border.glow
                        (EH.black |> EH.withAlpha 0.5)
                        5
                    ]

        Mobile ->
            [ [ [ logoBlock dProfile
                    |> el
                        [ alignTop
                        , Element.alignLeft
                        ]
                , maybeTxTracker dProfile model.trackedTxsExpanded model.trackedTxs
                    |> Maybe.map
                        (el
                            [ alignTop
                            , alignRight
                            ]
                        )
                    |> Maybe.withDefault none
                ]
                    |> column
                        [ alignTop
                        , Element.alignLeft
                        ]
              , connectButtonOrPhace dProfile (userInfo model.wallet) model.showAddressId
                    |> el
                        [ Element.centerY
                        , alignRight
                        ]
              ]
                |> row
                    [ fill
                        |> width
                    ]
            , [ navigationButtons model ]
                |> row
                    [ fill
                        |> width
                    ]
            ]
                |> column
                    [ fill
                        |> width
                    , Element.Background.color defaultTheme.headerBackground
                    , responsiveVal dProfile 20 10
                        |> padding
                    , responsiveVal dProfile 10 5
                        |> spacing
                    , Element.Border.glow
                        (EH.black |> EH.withAlpha 0.5)
                        5
                    ]


logoBlock :
    DisplayProfile
    -> Element Msg
logoBlock dProfile =
    [ Images.fryIcon
        |> Images.toElement
            [ Element.centerY
            , responsiveVal dProfile 60 30
                |> Element.px
                |> width
            ]
    , [ "Foundry Dashboard"
            |> text
            |> el
                [ Element.Font.color EH.white
                , responsiveVal dProfile 35 20
                    |> Element.Font.size
                , Element.Font.bold
                , Element.centerY
                ]
      , { url = "https://foundrydao.com"
        , label =
            "What is Foundry?"
                |> text
        }
            |> newTabLink
                [ Element.alignLeft
                , Element.Background.color Theme.blue
                , paddingXY 10 3
                , Element.Border.rounded 4
                , Element.Font.color EH.white
                , responsiveVal dProfile 18 10
                    |> Element.Font.size
                ]
      ]
        |> column
            [ spacing 5 ]
    ]
        |> row
            [ fill
                |> height
            , responsiveVal dProfile 10 7
                |> padding
            , responsiveVal dProfile 20 10
                |> spacing
            ]


navigationButtons :
    Model
    -> Element Msg
navigationButtons model =
    [ --  navigationButton
      --     Routing.Home
      --     model
      -- ,
      navigationButton
        Routing.Sentiment
        model
    , navigationButton
        Routing.Farm
        model
    , navigationButton
        Routing.Stats
        model
    , navigationButton
        Routing.Deth
        model
    ]
        |> row
            ([ responsiveVal model.dProfile 20 10
                |> spacing
             , Element.Font.color EH.white
             , responsiveVal model.dProfile 20 10
                |> Element.Font.size
             ]
                ++ (if model.dProfile == Mobile then
                        [ centerX ]

                    else
                        [ moveRight 50
                        , fill
                            |> width
                        ]
                   )
            )


navigationButton :
    Route
    -> Model
    -> Element Msg
navigationButton route model =
    { onPress =
        Navigate route
            |> Just
    , label =
        routeName route
            |> String.toUpper
            |> text
    }
        |> Element.Input.button
            ((if route == model.route then
                Theme.childContainerBackgroundAttributes
                    ++ Theme.childContainerBorderAttributes

              else
                Theme.mainContainerBackgroundAttributes
                    ++ Theme.mainContainerBorderAttributes
             )
                ++ [ responsiveVal model.dProfile 10 5
                        |> padding
                   ]
            )


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
                                , padding 5
                                ]
                       )
                )
                (EH.Action Types.ConnectToWeb3)

        Just userInfo ->
            phaceElement
                False
                userInfo.address
                (showAddressInfo == Just UserPhace)
                dProfile
                (ShowOrHideAddress UserPhace)
                Types.NoOp


userNoticeEls :
    DisplayProfile
    -> List UserNotice
    -> List (Element Msg)
userNoticeEls dProfile notices =
    if notices == [] then
        []

    else
        [ column
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
            , spacing <|
                EH.responsiveVal
                    dProfile
                    10
                    5
            , alignRight
            , alignBottom
            , width <|
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
                |> map (userNotice dProfile)
            )
        , column
            [ Element.moveRight <|
                EH.responsiveVal
                    dProfile
                    20
                    5
            , Element.moveDown 100
            , spacing <|
                EH.responsiveVal
                    dProfile
                    10
                    5
            , Element.alignLeft
            , alignTop
            , width <|
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
                |> map (userNotice dProfile)
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
                [ alignRight
                , alignTop
                , Element.moveUp 2
                ]
                EH.black
                (DismissNotice id)
    in
    el
        [ Element.Background.color color
        , Element.Border.rounded <|
            EH.responsiveVal
                dProfile
                10
                5
        , padding <|
            EH.responsiveVal
                dProfile
                8
                3
        , width fill
        , Element.Border.width 1
        , Element.Border.color <| rgba 0 0 0 0.15
        , EH.subtleShadow
        , EH.onClickNoPropagation Types.NoOp
        ]
        (notice.mainParagraphs
            |> map (map (Element.map never))
            |> List.indexedMap
                (\pNum paragraphLines ->
                    paragraph
                        [ width fill
                        , Element.Font.color textColor
                        , spacing 1
                        ]
                        (if pNum == 0 then
                            closeElement :: paragraphLines

                         else
                            paragraphLines
                        )
                )
            |> column
                [ spacing 4
                , width fill
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
                                el
                                    [ Element.Font.color <| trackedTxMiningColor ]
                                <|
                                    text <|
                                        String.fromInt n
                                            ++ " TXs mining"
                            )
                        )
                        (Maybe.map
                            (\n ->
                                el
                                    [ Element.Font.color <| trackedTxSuccessColor ]
                                <|
                                    text <|
                                        String.fromInt n
                                            ++ " TXs mined"
                            )
                        )
                        (Maybe.map
                            (\n ->
                                el
                                    [ Element.Font.color <| trackedTxFailedColor ]
                                <|
                                    text <|
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
                el
                    [ below <|
                        if showExpanded then
                            el
                                [ alignRight
                                , alignTop
                                ]
                            <|
                                trackedTxsColumn trackedTxs

                        else
                            none
                    ]
                <|
                    column
                        [ Element.Border.rounded 5
                        , Element.Border.width 2
                        , Element.Border.color Theme.blue
                        , Element.Background.color <| Element.rgb 0.2 0.2 0.2
                        , padding <|
                            responsiveVal
                                dProfile
                                10
                                5
                        , spacing <|
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
                            |> map (Maybe.withDefault none)
                        )


trackedTxsColumn :
    UserTx.Tracker Msg
    -> Element Msg
trackedTxsColumn trackedTxs =
    column
        [ Element.Background.color <| Theme.lightBlue
        , Element.Border.rounded 3
        , Element.Border.glow
            (rgba 0 0 0 0.2)
            4
        , padding 10
        , spacing 5
        , EH.onClickNoPropagation Types.NoOp
        , shrink
            |> maximum 400
            |> height
        , scrollbarY
        , alignRight
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
            newTabLink
                [ Element.Font.italic
                , Element.Font.color defaultTheme.linkTextColor
                ]
                { url = EthHelpers.etherscanTxUrl txHash
                , label = text label
                }

        titleEl =
            text <|
                case txInfo of
                    UserTx.StakingApprove ->
                        "Enable Farming Deposit"

                    UserTx.StakingDeposit amount ->
                        "Deposit "
                            ++ TokenValue.toConciseString amount
                            ++ " "
                            ++ Farm.liquidityDescription

                    UserTx.StakingWithdraw amount ->
                        "Withdraw "
                            ++ TokenValue.toConciseString amount
                            ++ " "
                            ++ Farm.liquidityDescription

                    UserTx.StakingClaim ->
                        "Claim FRY Rewards"

                    UserTx.StakingExit ->
                        "Exit Farming"

                    UserTx.DEthRedeem ->
                        "Redeem dETH"

                    UserTx.DEthDeposit ->
                        "Squander ETH"

                    UserTx.Send txh ->
                        "mining " ++ Eth.Utils.txHashToString txh

        statusEl =
            case signedTxStatus of
                UserTx.Mining ->
                    etherscanLink "Mining"

                UserTx.Failed ->
                    etherscanLink "Failed"

                UserTx.Success txReceipt ->
                    etherscanLink "Mined"
    in
    row
        [ width <| Element.px 300
        , Element.Background.color
            (signedTxStatusToColor signedTxStatus
                |> EH.withAlpha 0.3
            )
        , Element.Border.rounded 2
        , Element.Border.width 1
        , Element.Border.color <| rgba 0 0 0 0.3
        , padding 4
        , spacing 4
        , Element.Font.size 20
        ]
        [ titleEl
        , el [ alignRight ] <| statusEl
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
