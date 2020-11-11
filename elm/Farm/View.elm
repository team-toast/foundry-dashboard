module Farm.View exposing (..)

import Common.Types exposing (..)
import Common.View
import Config
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Eth.Types exposing (Address)
import Farm.Types exposing (..)
import FormatFloat
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
            { top =
                responsiveVal
                    dProfile
                    60
                    15
            , bottom = 0
            , left = 0
            , right = 0
            }
        ]
    <|
        Element.column
            [ Element.centerX
            , Element.spacing <|
                responsiveVal
                    dProfile
                    40
                    15
            ]
            [ titleEl dProfile
            , bodyEl dProfile model
            ]


titleEl : DisplayProfile -> Element Msg
titleEl dProfile =
    Element.el
        [ Element.Font.size <|
            responsiveVal
                dProfile
                50
                25
        , Element.Font.color EH.white
        , Element.Font.medium
        , Element.centerX
        ]
    <|
        Element.text "Farming for Fryers!"


bodyEl : DisplayProfile -> Model -> Element Msg
bodyEl dProfile model =
    let
        mainEl =
            case dProfile of
                EH.Desktop ->
                    Element.row

                EH.Mobile ->
                    Element.column

        balancesEl =
            balancesElement
                dProfile
                model.now
                model.wallet
                model.userStakingInfo
                model.depositWithdrawUXModel

        apyEl =
            apyElement
                dProfile
                model.apy
    in
    mainEl
        [ Element.centerX
        , Element.Background.color <| Element.rgb 0.6 0.6 1
        , Element.Border.rounded 10
        , Element.height <|
            Element.px <|
                responsiveVal
                    dProfile
                    500
                    400
        , Element.width <|
            Element.px <|
                responsiveVal
                    dProfile
                    800
                    300
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
        ]
        (case dProfile of
            EH.Desktop ->
                [ balancesEl
                , apyEl
                ]

            EH.Mobile ->
                [ apyEl
                , balancesEl
                ]
        )


balancesElement : DisplayProfile -> Time.Posix -> Wallet -> Maybe UserStakingInfo -> DepositOrWithdrawUXModel -> Element Msg
balancesElement dProfile now wallet maybeUserStakingInfo depositWithdrawUXModel =
    case Wallet.userInfo wallet of
        Nothing ->
            Common.View.web3ConnectButton
                dProfile
                [ Element.centerY ]
                MsgUp

        Just userInfo ->
            case maybeUserStakingInfo of
                Nothing ->
                    Element.el
                        [ Element.Font.italic
                        , Element.centerY
                        ]
                        (Element.text "Fetching info...")

                Just userStakingInfo ->
                    Element.column
                        [ Element.spacing <|
                            responsiveVal
                                dProfile
                                25
                                15
                        , Element.alignTop
                        ]
                        [ maybeGetLiquidityMessageElement
                            dProfile
                            userStakingInfo
                        , unstakedRow
                            dProfile
                            userStakingInfo
                            depositWithdrawUXModel
                        , stakedRow
                            dProfile
                            userStakingInfo
                            depositWithdrawUXModel
                        , rewardsRow
                            dProfile
                            userStakingInfo
                            now
                        ]


apyElement : DisplayProfile -> Maybe Float -> Element Msg
apyElement dProfile maybeApy =
    Element.el
        [ Element.alignTop
        , Element.alignRight
        , Element.Font.size <|
            responsiveVal
                dProfile
                30
                15
        ]
    <|
        case maybeApy of
            Nothing ->
                Element.el
                    [ Element.Font.italic ]
                    (Element.text "Fetching APY...")

            Just apy ->
                Element.column
                    [ Element.spacing 5
                    , Element.padding 5
                    , Element.Background.color <| Element.rgba 0 0 0 0.15
                    , Element.Border.rounded 5
                    , Element.Border.width 1
                    , Element.Border.color <| Element.rgba 0 0 0 0.1
                    ]
                    [ Element.text "Current APY: "
                    , Element.el
                        [ Element.centerX
                        , Element.Font.color <| Theme.darkBlue
                        , Element.Font.medium
                        ]
                      <|
                        Element.text <|
                            FormatFloat.formatFloat 2 apy
                                ++ "%"
                    ]


maybeGetLiquidityMessageElement : DisplayProfile -> UserStakingInfo -> Element Msg
maybeGetLiquidityMessageElement dProfile stakingInfo =
    if
        TokenValue.isZero stakingInfo.staked
            && TokenValue.isZero stakingInfo.unstaked
            && TokenValue.isZero stakingInfo.claimableRewards
    then
        Element.row
            [ Element.centerX
            , Element.Font.size <|
                responsiveVal
                    dProfile
                    20
                    15
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
    mainRow
        dProfile
        [ rowLabel
            dProfile
          <|
            responsiveVal
                dProfile
                "Unstaked Balance"
                "Unstaked ETHFRY"
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
        [ balanceOutputOrInput
            dProfile
            False
            stakingInfo.unstaked
            maybeDepositAmountUXModel
          <|
            responsiveVal
                dProfile
                "ETHFRY"
                ""
        , case maybeDepositAmountUXModel of
            Just depositAmountUX ->
                activeDepositUXButtons
                    dProfile
                    depositAmountUX
                    stakingInfo.unstaked

            Nothing ->
                inactiveUnstackedRowButtons
                    dProfile
                    stakingInfo
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
    mainRow
        dProfile
        [ rowLabel
            dProfile
          <|
            responsiveVal
                dProfile
                "Currently Staking"
                "Staked ETHFRY"
        , stakedRowUX
            dProfile
            stakingInfo
            maybeWithdrawAmountUXModel
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
        [ balanceOutputOrInput
            dProfile
            False
            stakingInfo.staked
            maybeWithdrawAmountUXModel
          <|
            responsiveVal
                dProfile
                "ETHFRY"
                ""
        , case maybeWithdrawAmountUXModel of
            Just withdrawAmountUX ->
                activeWithdrawUXButtons
                    dProfile
                    withdrawAmountUX
                    stakingInfo.staked

            Nothing ->
                stakedRowUXButtons
                    dProfile
                    stakingInfo.staked
        ]


stakedRowUXButtons : DisplayProfile -> TokenValue -> Element Msg
stakedRowUXButtons dProfile staked =
    buttonsRow
        dProfile
        [ maybeStartWithdrawButton
            dProfile
            staked
        , maybeExitButton
            dProfile
            staked
        ]


rewardsRow : DisplayProfile -> UserStakingInfo -> Time.Posix -> Element Msg
rewardsRow dProfile stakingInfo now =
    mainRow
        dProfile
        [ rowLabel
            dProfile
            "Available Rewards"
        , rewardsRowUX
            dProfile
            stakingInfo
            now
        ]


rewardsRowUX : DisplayProfile -> UserStakingInfo -> Time.Posix -> Element Msg
rewardsRowUX dProfile stakingInfo now =
    Element.row
        (rowUXStyles dProfile False)
        [ balanceOutputOrInput
            dProfile
            True
            (calcAvailableRewards
                stakingInfo
                now
            )
            Nothing
            "FRY"
        , if TokenValue.isZero <| calcAvailableRewards stakingInfo now then
            Element.none

          else
            claimRewardsButton
        ]


rowUXStyles : DisplayProfile -> Bool -> List (Element.Attribute Msg)
rowUXStyles dProfile isInput =
    [ Element.spacing <|
        responsiveVal
            dProfile
            30
            15
    , Element.width Element.fill
    , Element.padding 5
    , Element.height <| Element.px 50
    ]
        ++ (if isInput then
                [ Element.Border.rounded 5
                , Element.Background.color <| Element.rgba 1 1 1 0.3
                ]

            else
                []
           )


balanceOutputOrInput : DisplayProfile -> Bool -> TokenValue -> Maybe AmountUXModel -> String -> Element Msg
balanceOutputOrInput dProfile isRed balance maybeAmountUXModel tokenLabel =
    let
        amountElWidth =
            150
    in
    Element.row
        [ Element.spacing 10
        , Element.Font.size <|
            responsiveVal
                dProfile
                26
                16
        ]
        [ case maybeAmountUXModel of
            Just amountUXModel ->
                let
                    inputStyles =
                        [ Element.width <| Element.px amountElWidth
                        , Element.Background.color <| Element.rgba 1 1 1 0.3
                        , Element.padding 0
                        , Element.Border.width 0
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
            [ Element.width <| Element.px 100
            , Element.Font.color
                (if isRed then
                    Element.rgb 1 0 0

                 else
                    EH.black
                )
            ]
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
        dProfile
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
        dProfile
        [ depositButton
        , uxBackButton
        ]


buttonsRow : DisplayProfile -> List (Element Msg) -> Element Msg
buttonsRow dProfile =
    Element.row
        [ Element.spacing <|
            responsiveVal
                dProfile
                10
                5
        ]


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


mainRow : DisplayProfile -> List (Element Msg) -> Element Msg
mainRow dProfile =
    Element.column
        [ Element.spacing 5
        , Element.Background.color <| Element.rgba 1 1 1 0.1
        , Element.padding 10
        , Element.Border.rounded 5
        , Element.Border.width 1
        , Element.Border.color <| Element.rgba 0 0 0 0.1
        , Element.width <|
            responsiveVal
                dProfile
                (Element.px 420)
                Element.fill

        -- , Element.height <| Element.px 40
        , Element.Font.size <|
            responsiveVal
                dProfile
                30
                24
        ]


rowLabel : DisplayProfile -> String -> Element Msg
rowLabel dProfile text =
    Element.el
        [ Element.width <|
            responsiveVal
                dProfile
                (Element.px 280)
                Element.fill
        , Element.Font.size <|
            responsiveVal
                dProfile
                40
                20
        , Element.Font.semiBold
        ]
        (Element.text text)


commonImageAttributes : List (Attribute msg)
commonImageAttributes =
    [ Element.centerX
    , Element.centerY
    , Element.width <| Element.px <| 40
    ]


unlockButton : Element Msg
unlockButton =
    Element.el
        (actionButtonStyles
            (Just "Approve ETHFRY for Deposit")
            (Just DoUnlock)
        )
    <|
        Images.toElement
            commonImageAttributes
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
            commonImageAttributes
            Images.stakingDeposit


makeWithdrawButton : Maybe String -> Maybe Msg -> Element Msg
makeWithdrawButton maybeHoverText maybeOnClick =
    Element.el
        (actionButtonStyles maybeHoverText maybeOnClick)
    <|
        Images.toElement
            commonImageAttributes
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
            commonImageAttributes
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
            commonImageAttributes
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
            commonImageAttributes
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
