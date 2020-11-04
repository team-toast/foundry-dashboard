module Farm.View exposing (..)

import Common.Types exposing (..)
import Common.View
import Config
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Eth.Types exposing (Address)
import Farm.Types exposing (..)
import Helpers.Element as EH exposing (DisplayProfile, responsiveVal)
import Maybe.Extra
import Theme
import Time
import TokenValue exposing (TokenValue)
import Images


view : DisplayProfile -> Maybe UserInfo -> Model -> Element Msg
view dProfile maybeUserInfo model =
    Element.el
        [ Element.width Element.fill
        , Element.paddingEach
            { top = responsiveVal dProfile 60 30
            , bottom = 0
            , left = 0
            , right = 0
            }
        ]
    <|
        Element.column
            [ Element.centerX
            , Element.Background.color <| Element.rgb 0.3 0.3 0.3
            , Element.Border.rounded 10
            , Element.height <| Element.px <| responsiveVal dProfile 500 500
            , Element.width <| Element.px <| responsiveVal dProfile 700 200
            ]
            [ Element.el
                [ Element.Events.onClick FakeFetchBalanceInfo
                ]
                (Element.text "clicky")
            , case maybeUserInfo of
                Nothing ->
                    Common.View.web3ConnectButton
                        dProfile
                        [ Element.centerX
                        , Element.centerY
                        ]
                        MsgUp

                Just userInfo ->
                    case model.timedUserStakingInfo of
                        Nothing ->
                            Element.el
                                [ Element.centerX
                                , Element.centerY
                                , Element.Font.italic
                                ]
                                (Element.text "Fetching info...")

                        Just timedUserStakingInfo ->
                            Element.column
                                [ Element.spacing 15
                                , Element.centerX
                                ]
                                [ unstakedBalanceRow dProfile timedUserStakingInfo.userStakingInfo.unstaked
                                , depositWithrawUX dProfile userInfo.address timedUserStakingInfo.userStakingInfo model.depositWithdrawUXModel
                                , stakedBalanceRow dProfile timedUserStakingInfo.userStakingInfo.staked
                                , rewardsAvailableRowAndUX dProfile userInfo.address timedUserStakingInfo model.now
                                ]
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


depositWithrawUX : DisplayProfile -> Address -> UserStakingInfo -> DepositWithdrawUXModel -> Element Msg
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
                            Nothing

                        else
                            Just <|
                                makeDepositButton StartDeposit

                    maybeWithdrawStartButton =
                        if TokenValue.isZero balanceInfo.staked then
                            Nothing

                        else
                            Just <|
                                makeWithdrawButton StartWithdraw
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
makeDepositButton onClick =
    Element.el
        (depositWithdrawButtonStyles onClick)
    <|
        Images.toElement
            [ Element.centerX
            , Element.centerY
            , Element.width <| Element.px <| 40
            ]
            Images.stakingDeposit


makeWithdrawButton : Msg -> Element Msg
makeWithdrawButton onClick =
    Element.el
        (depositWithdrawButtonStyles onClick)
    <|
        Images.toElement
            [ Element.centerX
            , Element.centerY
            , Element.width <| Element.px <| 40
            ]
            Images.stakingExit


depositWithdrawButtonStyles : Msg -> List (Element.Attribute Msg)
depositWithdrawButtonStyles onClick =
    [ Element.width <| Element.px 50
    , Element.height <| Element.px 50
    , Element.pointer
    , Element.Events.onClick onClick
    ]


stakedBalanceRow : DisplayProfile -> TokenValue -> Element Msg
stakedBalanceRow dProfile stakedBalance =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 10
        ]
        [ balanceLabel dProfile "Currently Staking"
        , balanceOutput dProfile stakedBalance "ETHFRY"
        ]


rewardsAvailableRowAndUX : DisplayProfile -> Address -> TimedUserStakingInfo -> Time.Posix -> Element Msg
rewardsAvailableRowAndUX dProfile userAddress balanceInfo now =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 10
        ]
        [ balanceLabel dProfile "Available Rewards"
        , balanceOutput
            dProfile
            (calcAvailableRewards
                balanceInfo
                now
            )
            "FRY"
        ]


balanceLabel : DisplayProfile -> String -> Element Msg
balanceLabel dProfile text =
    Element.el
        [ Element.Font.size <| responsiveVal dProfile 30 24
        , Element.width <| Element.px <| responsiveVal dProfile 280 240 ]
        (Element.text text)


balanceOutput : DisplayProfile -> TokenValue -> String -> Element Msg
balanceOutput dProfile amount label =
    Element.row
        [ Element.Font.size <| responsiveVal dProfile 30 24
        , Element.spacing 4
        ]
        [ Element.text <| TokenValue.toConciseString amount
        , Element.text label
        ]
