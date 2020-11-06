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
            [ -- Element.el
              -- [ Element.Events.onClick FakeFetchBalanceInfo
              -- ]
              -- (Element.text "clicky")
              -- ,
              case maybeUserInfo of
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
                                [ maybeGetLiquidityMessageElement dProfile userStakingInfo
                                , unstakedRow dProfile userStakingInfo model.depositWithdrawUXModel userInfo
                                -- , stakedBalanceRow dProfile userStakingInfo.staked model.depositWithdrawUXModel
                                -- , rewardsAvailableRowAndUX dProfile userStakingInfo model.now
                                ]
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


unstakedRow : DisplayProfile -> UserStakingInfo -> DepositOrWithdrawUXModel -> UserInfo -> Element Msg
unstakedRow dProfile userStakingInfo depositOrWithdrawUXModel userInfo =
    let
        maybeDepositAmountUXModel =
            case depositOrWithdrawUXModel of
                Just ( Deposit, amountUXModel ) ->
                    Just amountUXModel

                _ ->
                    Nothing
    in
    mainRow
        [ rowLabel dProfile "Unstaked Balance"
        , unstakedRowUX dProfile userStakingInfo maybeDepositAmountUXModel
        ]


unstakedRowUX : DisplayProfile -> UserStakingInfo -> Maybe AmountUXModel -> Element Msg
unstakedRowUX dProfile userStakingInfo maybeDepositAmountUXModel =
    let
        rowStyles =
            [ Element.spacing 30
            , Element.width Element.fill
            , Element.padding 5
            ]
                ++ (case maybeDepositAmountUXModel of
                        Just _ ->
                            [ Element.Border.rounded 5
                            , Element.Background.color <| Element.rgba 1 1 1 0.3
                            ]

                        Nothing ->
                            []
                   )
    in
    Element.row
        rowStyles
        [ balanceOutputOrInput dProfile
            userStakingInfo.unstaked
            maybeDepositAmountUXModel
            "ETHFRY"

        -- (balanceOutput dProfile userStakingInfo.unstaked "ETHFRY")
        , case maybeDepositAmountUXModel of
            Just depositAmountUX ->
                activeDepositUXButtons dProfile depositAmountUX

            Nothing ->
                depositExitUXButtons dProfile userStakingInfo maybeDepositAmountUXModel
        ]


balanceOutputOrInput : DisplayProfile -> TokenValue -> Maybe AmountUXModel -> String -> Element Msg
balanceOutputOrInput dProfile unstaked maybeAmountUXModel tokenLabel =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 10
        ]
        [ case maybeAmountUXModel of
            Just amountUXModel ->
                Element.Input.text
                    [ Element.width Element.fill
                    , Element.Background.color <| Element.rgba 1 1 1 0.3
                    , Element.height Element.fill
                    ]
                    { onChange = AmountInputChanged
                    , text = amountUXModel.amountInput
                    , placeholder = Nothing
                    , label = Element.Input.labelHidden "amount"
                    }

            Nothing ->
                Element.text <| TokenValue.toFloatString Nothing <| unstaked
        , Element.text tokenLabel
        ]


activeDepositUXButtons : DisplayProfile -> AmountUXModel -> Element Msg
activeDepositUXButtons dProfile amountUXModel =
    buttonsRow
        [ makeDepositButton (Maybe.map DoDeposit (validateInput amountUXModel.amountInput))
        , uxBackButton
        ]


buttonsRow =
    Element.row [ Element.spacing 10 ]


depositExitUXButtons : DisplayProfile -> UserStakingInfo -> Maybe AmountUXModel -> Element Msg
depositExitUXButtons dProfile stakingInfo maybeAmountUXModel =
    case maybeAmountUXModel of
        Nothing ->
            let
                maybeUnlockOrDepositStartButton =
                    if TokenValue.isZero stakingInfo.unstaked then
                        Nothing

                    else if TokenValue.isZero stakingInfo.allowance then
                        Just unlockButton

                    else
                        Just <|
                            makeDepositButton <|
                                Just <| StartDeposit stakingInfo.unstaked

                maybeWithdrawStartButton =
                    if TokenValue.isZero stakingInfo.staked then
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
                    [ maybeUnlockOrDepositStartButton
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


-- withdrawUX : DisplayProfile -> Maybe AmountUXModel -> Element Msg
-- withdrawUX dProfile maybeAmountUXModel =
--     case maybeAmountUXModel of
--         Nothing ->
--             makeWithdrawButton <| Just StartWithdraw

--         Just amountUXModel ->
--             Element.row
--                 [ Element.spacing 5 ]
--                 [ amountInputField amountUXModel
--                 , makeWithdrawButton
--                     (Maybe.map DoWithdraw (validateInput amountUXModel.amountInput))
--                 ]


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


-- stakedBalanceRow : DisplayProfile -> TokenValue -> DepositOrWithdrawUXModel -> Element Msg
-- stakedBalanceRow dProfile stakedBalance depositOrWithdrawUXModel =
--     mainRow
--         [ rowLabel dProfile "Currently Staking"
--         , balanceOutput dProfile stakedBalance "ETHFRY"
--         , if TokenValue.isZero stakedBalance then
--             Element.none

--           else
--             let
--                 maybeWithdrawAmountUXModel =
--                     case depositOrWithdrawUXModel of
--                         Just ( Withdraw, amountUXModel ) ->
--                             Just amountUXModel

--                         _ ->
--                             Nothing
--             in
--             withdrawUX dProfile maybeWithdrawAmountUXModel
--         ]


-- rewardsAvailableRowAndUX : DisplayProfile -> UserStakingInfo -> Time.Posix -> Element Msg
-- rewardsAvailableRowAndUX dProfile stakingInfo now =
--     mainRow
--         [ rowLabel dProfile "Available Rewards"
--         , balanceOutput
--             dProfile
--             (calcAvailableRewards
--                 stakingInfo
--                 now
--             )
--             "FRY"
--         , if TokenValue.isZero stakingInfo.claimableRewards then
--             Element.none

--           else
--             claimRewardsButton
--         ]


mainRow : List (Element Msg) -> Element Msg
mainRow =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 30
        , Element.height <| Element.px 40
        ]


rowLabel : DisplayProfile -> String -> Element Msg
rowLabel dProfile text =
    Element.el
        [ Element.Font.size <| responsiveVal dProfile 30 24
        , Element.Font.alignRight
        , Element.width <| Element.px <| responsiveVal dProfile 280 240
        ]
        (Element.text text)



-- balanceOutput : DisplayProfile -> TokenValue -> String -> Element Msg
-- balanceOutput dProfile amount label =
--     Element.row
--         [ Element.Font.size <| responsiveVal dProfile 30 24
--         , Element.spacing 4
--         , Element.width <| Element.px 200
--         ]
--         [ Element.text <| TokenValue.toConciseString amount
--         , Element.text label
--         ]


unlockButton : Element Msg
unlockButton =
    Element.el
        (actionButtonStyles <| Just DoUnlock)
        (Element.text "U")


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


claimRewardsButton : Element Msg
claimRewardsButton =
    Element.el
        (actionButtonStyles <| Just DoClaimRewards)
        (Element.text "R")


uxBackButton : Element Msg
uxBackButton =
    Element.el
        (actionButtonStyles <| Just UXBack)
        (Element.text "B")


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
