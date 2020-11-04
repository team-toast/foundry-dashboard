module Farm.View exposing (..)

import Common.Types exposing (..)
import Common.View
import Config
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Eth.Types exposing (Address)
import Farm.Types exposing (..)
import Helpers.Element as EH exposing (DisplayProfile, responsiveVal)
import Maybe.Extra
import Theme
import TokenValue exposing (TokenValue)


view : DisplayProfile -> Maybe UserInfo -> Model -> Element Msg
view dProfile maybeUserInfo model =
    Element.el
        [ Element.centerX
        , Element.paddingEach
            { top = responsiveVal dProfile 30 10
            , bottom = 0
            , left = 0
            , right = 0
            }
        , Element.Background.color EH.white
        , Element.Border.rounded 10
        , Element.height <| Element.px <| responsiveVal dProfile 500 500
        ]
    <|
        case maybeUserInfo of
            Nothing ->
                Common.View.web3ConnectButton
                    dProfile
                    [ Element.centerX
                    , Element.centerY
                    ]
                    MsgUp

            Just userInfo ->
                case model.userBalanceInfo of
                    Nothing ->
                        Element.el
                            [ Element.centerX
                            , Element.centerY
                            , Element.Font.italic
                            , Element.Font.color Theme.darkGray
                            ]
                            (Element.text "Fetching info...")

                    Just userBalanceInfo ->
                        Element.column
                            [ Element.spacing 10
                            , Element.width Element.fill
                            ]
                            [ unstakedBalanceRow dProfile userBalanceInfo.unstaked
                            , depositWithrawUX dProfile userInfo.address userBalanceInfo model.depositWithdrawUXModel
                            , stakedBalanceRow dProfile userBalanceInfo.staked
                            , rewardsAvailableRowAndUX dProfile userInfo.address userBalanceInfo
                            ]


unstakedBalanceRow : DisplayProfile -> TokenValue -> Element Msg
unstakedBalanceRow dProfile unstakedBalance =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 10
        ]
        [ balanceLabel dProfile "Unstaked Balance"
        , balanceOutput dProfile unstakedBalance "ETHFRY"
        ]


depositWithrawUX : DisplayProfile -> Address -> UserBalanceInfo -> DepositWithdrawUXModel -> Element Msg
depositWithrawUX dProfile userAddress balanceInfo uxModel =
    if TokenValue.isZero balanceInfo.staked && TokenValue.isZero balanceInfo.unstaked then
        Element.row
            [ Element.centerX
            ]
            [ Element.newTabLink
                [ Element.Font.color Theme.blue ]
                { url = Config.urlToLiquidityPool
                , label = Element.text "Obtain ETHFRY Liquidity"
                }
            , Element.text " to continue."
            ]

    else
        case uxModel.inMenu of
            Nothing ->
                let
                    maybeDepositStartButton =
                        if TokenValue.isZero balanceInfo.unstaked then
                            Just <|
                                makeDepositButton StartDeposit

                        else
                            Nothing

                    maybeWithdrawStartButton =
                        if TokenValue.isZero balanceInfo.staked then
                            Just <|
                                makeWithdrawButton StartWithdraw

                        else
                            Nothing
                in
                Element.row
                    [ Element.centerX
                    , Element.spacing 10
                    ]
                <|
                    Maybe.Extra.values
                        [ maybeDepositStartButton
                        , maybeWithdrawStartButton
                        ]
            Just inMenu ->
                Debug.todo ""


makeDepositButton : Msg -> Element Msg
makeDepositButton onclick =
    Debug.todo ""


makeWithdrawButton : Msg -> Element Msg
makeWithdrawButton onclick =
    Debug.todo ""


stakedBalanceRow : DisplayProfile -> TokenValue -> Element Msg
stakedBalanceRow dProfile stakedBalance =
    Debug.todo ""


rewardsAvailableRowAndUX : DisplayProfile -> Address -> UserBalanceInfo -> Element Msg
rewardsAvailableRowAndUX dProfile userAddress balanceInfo =
    Debug.todo ""


balanceLabel : DisplayProfile -> String -> Element Msg
balanceLabel dProfile text =
    Debug.todo ""


balanceOutput : DisplayProfile -> TokenValue -> String -> Element Msg
balanceOutput dProfile amount label =
    Debug.todo ""
