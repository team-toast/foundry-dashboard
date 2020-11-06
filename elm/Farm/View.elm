module Farm.View exposing (..)

import Common.Types exposing (..)
import Common.View
import Config
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Eth.Types exposing (Address)
import Farm.Types exposing (..)
import Helpers.Element as EH exposing (DisplayProfile, responsiveVal)
import Images
import Maybe.Extra
import Theme
import Time
import TokenValue exposing (TokenValue)


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
            , Element.Background.color <| Element.rgb 0.6 0.6 1
            , Element.Border.rounded 10
            , Element.height <| Element.px <| responsiveVal dProfile 500 500
            , Element.width <| Element.px <| responsiveVal dProfile 700 200

            -- , Element.Font.color EH.white
            -- , Element.Border.glow (Element.rgba 1 1 1 0.2) 6
            -- , Element.Border.width 3
            -- , Element.Border.color EH.black
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
                    case model.userStakingInfo of
                        Nothing ->
                            Element.el
                                [ Element.centerX
                                , Element.centerY
                                , Element.Font.italic
                                ]
                                (Element.text "Fetching info...")

                        Just userStakingInfo ->
                            Element.column
                                [ Element.spacing 15
                                ]
                                [ unstakedBalanceRow dProfile userStakingInfo model.depositWithdrawUXModel userInfo
                                , maybeGetLiquidityMessageElement dProfile userStakingInfo
                                , stakedBalanceRow dProfile userStakingInfo.staked model.depositWithdrawUXModel
                                , rewardsAvailableRowAndUX dProfile userStakingInfo model.now
                                ]
            ]


unstakedBalanceRow : DisplayProfile -> UserStakingInfo -> DepositOrWithdrawUXModel -> UserInfo -> Element Msg
unstakedBalanceRow dProfile userStakingInfo depositOrWithdrawUXModel userInfo =
    let
        maybeDepositAmountUXModel =
            case depositOrWithdrawUXModel of
                Just ( Deposit, amountUXModel ) ->
                    Just amountUXModel

                _ ->
                    Nothing
    in
    mainRow
        [ balanceLabel dProfile "Unstaked Balance"
        , balanceOutput dProfile userStakingInfo.unstaked "ETHFRY"
        , depositExitUX dProfile userInfo.address userStakingInfo maybeDepositAmountUXModel
        ]


maybeGetLiquidityMessageElement : DisplayProfile -> UserStakingInfo -> Element Msg
maybeGetLiquidityMessageElement dProfile stakingInfo =
    if TokenValue.isZero stakingInfo.staked && TokenValue.isZero stakingInfo.unstaked && TokenValue.isZero stakingInfo.claimableRewards then
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
        Element.none


depositExitUX : DisplayProfile -> Address -> UserStakingInfo -> Maybe AmountUXModel -> Element Msg
depositExitUX dProfile userAddress balanceInfo maybeAmountUXModel =
    case maybeAmountUXModel of
        Nothing ->
            let
                maybeDepositStartButton =
                    if TokenValue.isZero balanceInfo.unstaked then
                        Nothing

                    else
                        Just <|
                            makeDepositButton <|
                                Just StartDeposit

                maybeWithdrawStartButton =
                    if TokenValue.isZero balanceInfo.staked then
                        Nothing

                    else
                        Just <|
                            exitButton
            in
            Element.row
                [ Element.spacing 10
                ]
            <|
                Maybe.Extra.values
                    [ Just unlockButton
                    , maybeDepositStartButton
                    , maybeWithdrawStartButton
                    ]

        Just amountUXModel ->
            Element.row
                [ Element.spacing 5
                ]
                [ amountInputField amountUXModel
                , makeDepositButton
                    (Maybe.map DoDeposit (validateInput amountUXModel.amountInput))
                ]


withdrawUX : DisplayProfile -> Maybe AmountUXModel -> Element Msg
withdrawUX dProfile maybeAmountUXModel =
    case maybeAmountUXModel of
        Nothing ->
            makeWithdrawButton <| Just StartWithdraw

        Just amountUXModel ->
            Element.row
                [ Element.spacing 5 ]
                [ amountInputField amountUXModel
                , makeWithdrawButton
                    (Maybe.map DoDeposit (validateInput amountUXModel.amountInput))
                ]


amountInputField : AmountUXModel -> Element Msg
amountInputField amountUXModel =
    Element.Input.text
        [ Element.width <| Element.px 200
        , Element.height Element.fill
        ]
        { onChange = AmountInputChanged
        , text = amountUXModel.amountInput
        , placeholder = Nothing
        , label = Element.Input.labelHidden "amount"
        }


makeDepositButton : Maybe Msg -> Element Msg
makeDepositButton maybeOnClick =
    Element.el
        (actionButtonStyles maybeOnClick)
    <|
        Images.toElement
            [ Element.centerX
            , Element.centerY
            , Element.width <| Element.px <| 40
            ]
            Images.stakingDeposit


makeWithdrawButton : Maybe Msg -> Element Msg
makeWithdrawButton maybeOnClick =
    Element.el
        (actionButtonStyles maybeOnClick)
    <|
        Element.text "W"



-- Images.toElement
--     [ Element.centerX
--     , Element.centerY
--     , Element.width <| Element.px <| 40
--     ]
--     Images.stakingWithdraw


exitButton : Element Msg
exitButton =
    Element.el
        (actionButtonStyles <| Just DoExit)
    <|
        Images.toElement
            [ Element.centerX
            , Element.centerY
            , Element.width <| Element.px <| 40
            ]
            Images.stakingExit

unlockButton : Element Msg
unlockButton =
    Element.el
        (actionButtonStyles <| Just DoUnlock)
        (Element.text "U")


actionButtonStyles : Maybe Msg -> List (Element.Attribute Msg)
actionButtonStyles maybeOnClick =
    [ Element.width <| Element.px 45
    , Element.height <| Element.px 45
    , Element.Border.rounded 6
    , Element.Border.width 1
    , Element.Border.color <| Element.rgba 0 0 0 0.2
    ]
        ++ (case maybeOnClick of
                Just onClick ->
                    [ Element.pointer
                    , Element.Events.onClick onClick
                    , Element.Background.color <| Element.rgba 1 1 1 0.2
                    ]

                Nothing ->
                    [ Element.Background.color <| Element.rgb 0.7 0.7 0.7 ]
           )


stakedBalanceRow : DisplayProfile -> TokenValue -> DepositOrWithdrawUXModel -> Element Msg
stakedBalanceRow dProfile stakedBalance depositOrWithdrawUXModel =
    mainRow
        [ balanceLabel dProfile "Currently Staking"
        , balanceOutput dProfile stakedBalance "ETHFRY"
        , if TokenValue.isZero stakedBalance then
            Element.none

          else
            let
                maybeWithdrawAmountUXModel =
                    case depositOrWithdrawUXModel of
                        Just ( Withdraw, amountUXModel ) ->
                            Just amountUXModel

                        _ ->
                            Nothing
            in
            withdrawUX dProfile maybeWithdrawAmountUXModel
        ]


rewardsAvailableRowAndUX : DisplayProfile -> UserStakingInfo -> Time.Posix -> Element Msg
rewardsAvailableRowAndUX dProfile stakingInfo now =
    mainRow
        [ balanceLabel dProfile "Available Rewards"
        , balanceOutput
            dProfile
            (calcAvailableRewards
                stakingInfo
                now
            )
            "FRY"
        ]


mainRow : List (Element Msg) -> Element Msg
mainRow =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 30
        , Element.height <| Element.px 40
        ]


balanceLabel : DisplayProfile -> String -> Element Msg
balanceLabel dProfile text =
    Element.el
        [ Element.Font.size <| responsiveVal dProfile 30 24
        , Element.Font.alignRight
        , Element.width <| Element.px <| responsiveVal dProfile 280 240
        ]
        (Element.text text)


balanceOutput : DisplayProfile -> TokenValue -> String -> Element Msg
balanceOutput dProfile amount label =
    Element.row
        [ Element.Font.size <| responsiveVal dProfile 30 24
        , Element.spacing 4
        , Element.width <| Element.px 200
        ]
        [ Element.text <| TokenValue.toConciseString amount
        , Element.text label
        ]
