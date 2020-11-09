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
import Wallet exposing (Wallet)


view : DisplayProfile -> Model -> Element Msg
view dProfile model =
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
            , Element.width <| Element.px <| responsiveVal dProfile 800 200
            , Element.padding 20
            ]
            [ case Wallet.userInfo model.wallet of
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
                                [ Element.spacing 25
                                ]
                                [ maybeGetLiquidityMessageElement dProfile userStakingInfo
                                , unstakedRow dProfile userStakingInfo model.depositWithdrawUXModel
                                , stakedRow dProfile userStakingInfo model.depositWithdrawUXModel
                                , rewardsRow dProfile userStakingInfo model.now
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


unstakedRow : DisplayProfile -> UserStakingInfo -> DepositOrWithdrawUXModel -> Element Msg
unstakedRow dProfile userStakingInfo depositOrWithdrawUXModel =
    let
        maybeDepositAmountUXModel =
            case depositOrWithdrawUXModel of
                Just ( Deposit, amountUXModel ) ->
                    Just amountUXModel

                _ ->
                    Nothing
    in
    mainRow dProfile
        [ rowLabel dProfile "Unstaked Balance"
        , unstakedRowUX dProfile userStakingInfo maybeDepositAmountUXModel
        ]


unstakedRowUX : DisplayProfile -> UserStakingInfo -> Maybe AmountUXModel -> Element Msg
unstakedRowUX dProfile stakingInfo maybeDepositAmountUXModel =
    let
        rowStyles =
            let
                isInput =
                    case maybeDepositAmountUXModel of
                        Just _ ->
                            True

                        _ ->
                            False
            in
            rowUXStyles dProfile isInput
    in
    Element.row
        rowStyles
        [ balanceOutputOrInput dProfile
            stakingInfo.unstaked
            maybeDepositAmountUXModel
            "ETHFRY"
        , case maybeDepositAmountUXModel of
            Just depositAmountUX ->
                activeDepositUXButtons dProfile depositAmountUX stakingInfo.unstaked

            Nothing ->
                inactiveUnstackedRowButtons dProfile stakingInfo
        ]


stakedRow : DisplayProfile -> UserStakingInfo -> DepositOrWithdrawUXModel -> Element Msg
stakedRow dProfile stakingInfo depositOrWithdrawUXModel =
    let
        maybeWithdrawAmountUXModel =
            case depositOrWithdrawUXModel of
                Just ( Withdraw, amountUXModel ) ->
                    Just amountUXModel

                _ ->
                    Nothing
    in
    mainRow dProfile
        [ rowLabel dProfile "Currently Staking"
        , stakedRowUX dProfile stakingInfo maybeWithdrawAmountUXModel
        ]


stakedRowUX : DisplayProfile -> UserStakingInfo -> Maybe AmountUXModel -> Element Msg
stakedRowUX dProfile stakingInfo maybeWithdrawAmountUXModel =
    let
        rowStyles =
            let
                isInput =
                    case maybeWithdrawAmountUXModel of
                        Just _ ->
                            True

                        _ ->
                            False
            in
            rowUXStyles dProfile isInput
    in
    Element.row
        rowStyles
        [ balanceOutputOrInput dProfile
            stakingInfo.staked
            maybeWithdrawAmountUXModel
            "ETHFRY"
        , case maybeWithdrawAmountUXModel of
            Just withdrawAmountUX ->
                activeWithdrawUXButtons dProfile withdrawAmountUX stakingInfo.staked

            Nothing ->
                stakedRowUXButtons dProfile stakingInfo.staked
        ]


stakedRowUXButtons : DisplayProfile -> TokenValue -> Element Msg
stakedRowUXButtons dProfile staked =
    buttonsRow
        [ maybeStartWithdrawButton dProfile staked
        , maybeExitButton dProfile staked
        ]


rewardsRow : DisplayProfile -> UserStakingInfo -> Time.Posix -> Element Msg
rewardsRow dProfile stakingInfo now =
    mainRow dProfile
        [ rowLabel dProfile "Available Rewards"
        , rewardsRowUX dProfile stakingInfo now
        ]


rewardsRowUX : DisplayProfile -> UserStakingInfo -> Time.Posix -> Element Msg
rewardsRowUX dProfile stakingInfo now =
    Element.row
        (rowUXStyles dProfile False)
        [ balanceOutputOrInput dProfile
            (calcAvailableRewards
                stakingInfo
                now
            )
            Nothing
            "FRY"
        , if TokenValue.isZero stakingInfo.claimableRewards then
            Element.none

          else
            claimRewardsButton
        ]


rowUXStyles : DisplayProfile -> Bool -> List (Element.Attribute Msg)
rowUXStyles dProfile isInput =
    [ Element.spacing 30
    , Element.width Element.fill
    , Element.padding 5
    ]
        ++ (if isInput then
                [ Element.Border.rounded 5
                , Element.Background.color <| Element.rgba 1 1 1 0.3
                ]

            else
                []
           )


balanceOutputOrInput : DisplayProfile -> TokenValue -> Maybe AmountUXModel -> String -> Element Msg
balanceOutputOrInput dProfile balance maybeAmountUXModel tokenLabel =
    let
        amountElWidth =
            200
    in
    Element.row
        [ Element.spacing 10
        ]
        [ case maybeAmountUXModel of
            Just amountUXModel ->
                let
                    inputStyles =
                        [ Element.width <| Element.px amountElWidth
                        , Element.Background.color <| Element.rgba 1 1 1 0.3
                        , Element.height Element.fill
                        ]
                            ++ (if validateInput amountUXModel.amountInput balance == Nothing then
                                    [ Element.Border.width 2
                                    , Element.Border.color <| Theme.darkRed
                                    ]

                                else
                                    []
                               )
                in
                Element.Input.text
                    inputStyles
                    { onChange = AmountInputChanged
                    , text = amountUXModel.amountInput
                    , placeholder = Nothing
                    , label = Element.Input.labelHidden "amount"
                    }

            Nothing ->
                Element.el
                    [ Element.width <| Element.px amountElWidth
                    , Element.clip
                    ]
                    (Element.text <|
                        TokenValue.toFloatString Nothing <|
                            balance
                    )
        , Element.el
            [ Element.width <| Element.px 100 ]
          <|
            Element.text tokenLabel
        ]


activeWithdrawUXButtons : DisplayProfile -> AmountUXModel -> TokenValue -> Element Msg
activeWithdrawUXButtons dProfile amountUXModel stakedBalance =
    let
        withdrawButton =
            case validateInput amountUXModel.amountInput stakedBalance of
                Just amount ->
                    makeWithdrawButton
                        (Just <|
                            "Withdraw "
                                ++ TokenValue.toConciseString amount
                                ++ " ETHFRY"
                        )
                        (Just <| DoWithdraw amount)

                Nothing ->
                    makeWithdrawButton Nothing Nothing
    in
    buttonsRow
        [ withdrawButton
        , uxBackButton
        ]


activeDepositUXButtons : DisplayProfile -> AmountUXModel -> TokenValue -> Element Msg
activeDepositUXButtons dProfile amountUXModel unstakedBalance =
    let
        depositButton =
            case validateInput amountUXModel.amountInput unstakedBalance of
                Just amount ->
                    makeDepositButton
                        (Just <|
                            "Deposit "
                                ++ TokenValue.toConciseString amount
                                ++ " ETHFRY"
                        )
                        (Just <| DoDeposit amount)

                Nothing ->
                    makeDepositButton Nothing Nothing
    in
    buttonsRow
        [ depositButton
        , uxBackButton
        ]


buttonsRow : List (Element Msg) -> Element Msg
buttonsRow =
    Element.row [ Element.spacing 10 ]


maybeStartWithdrawButton : DisplayProfile -> TokenValue -> Element Msg
maybeStartWithdrawButton dProfile currentBalance =
    if TokenValue.isZero currentBalance then
        Element.none

    else
        makeWithdrawButton
            (Just "Withdraw ETHFRY")
            (Just <| StartWithdraw currentBalance)


maybeExitButton : DisplayProfile -> TokenValue -> Element Msg
maybeExitButton dProfile stakedAmount =
    if TokenValue.isZero stakedAmount then
        Element.none

    else
        exitButton dProfile


inactiveUnstackedRowButtons : DisplayProfile -> UserStakingInfo -> Element Msg
inactiveUnstackedRowButtons dProfile stakingInfo =
    if TokenValue.isZero stakingInfo.unstaked then
        Element.none

    else if TokenValue.isZero stakingInfo.allowance then
        unlockButton

    else
        makeDepositButton
            (Just "Deposit ETHFRY")
            (Just <| StartDeposit stakingInfo.unstaked)


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


mainRow : DisplayProfile -> List (Element Msg) -> Element Msg
mainRow dProfile =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 30
        , Element.height <| Element.px 40
        , Element.Font.size <| responsiveVal dProfile 30 24
        ]


rowLabel : DisplayProfile -> String -> Element Msg
rowLabel dProfile text =
    Element.el
        [ Element.width <| Element.px <| responsiveVal dProfile 280 240
        ]
        (Element.text text)


unlockButton : Element Msg
unlockButton =
    Element.el
        (actionButtonStyles
            (Just "Approve ETHFRY for Deposit")
            (Just DoUnlock)
        )
    <|
        Images.toElement
            [ Element.centerX
            , Element.centerY
            , Element.width <| Element.px <| 40
            ]
            Images.unlock


makeDepositButton : Maybe String -> Maybe Msg -> Element Msg
makeDepositButton maybeHoverText maybeOnClick =
    Element.el
        (actionButtonStyles
            maybeHoverText
            maybeOnClick
        )
    <|
        Images.toElement
            [ Element.centerX
            , Element.centerY
            , Element.width <| Element.px <| 40
            ]
            Images.stakingDeposit


makeWithdrawButton : Maybe String -> Maybe Msg -> Element Msg
makeWithdrawButton maybeHoverText maybeOnClick =
    Element.el
        (actionButtonStyles maybeHoverText maybeOnClick)
    <|
        Images.toElement
            [ Element.centerX
            , Element.centerY
            , Element.width <| Element.px <| 40
            ]
            Images.stakingWithdraw


exitButton : DisplayProfile -> Element Msg
exitButton dProfile =
    Element.el
        (actionButtonStyles
            (Just "Exit with all assets (FRY and ETHFRY)")
            (Just DoExit)
        )
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
        (actionButtonStyles
            (Just "Claim FRY Rewards")
            (Just DoClaimRewards)
        )
    <|
        Images.toElement
            [ Element.centerX
            , Element.centerY
            , Element.width <| Element.px <| 40
            ]
            Images.stakingClaimReward


uxBackButton : Element Msg
uxBackButton =
    Element.el
        (actionButtonStyles
            (Just "Back")
            (Just UXBack)
        )
    <|
        Images.toElement
            [ Element.centerX
            , Element.centerY
            , Element.width <| Element.px <| 40
            ]
            Images.back


actionButtonStyles : Maybe String -> Maybe Msg -> List (Element.Attribute Msg)
actionButtonStyles maybeHoverText maybeOnClick =
    [ Element.width <| Element.px 45
    , Element.height <| Element.px 45
    , Element.Border.rounded 6
    , Element.Border.width 1
    , Element.Border.color <| Element.rgba 0 0 0 0.2
    ]
        ++ Maybe.Extra.values
            [ Maybe.map EH.withTitle maybeHoverText ]
        ++ (case maybeOnClick of
                Just onClick ->
                    [ Element.pointer
                    , Element.Events.onClick onClick
                    , Element.Background.color <| Element.rgba 1 1 1 0.2
                    ]

                Nothing ->
                    [ Element.Background.color <| Element.rgb 0.7 0.7 0.7 ]
           )
