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
import Helpers.Element as EH exposing (DisplayProfile, redButton, responsiveVal)
import Helpers.Time as TimeHelpers
import Images
import Maybe.Extra
import Theme
import Time
import TokenValue exposing (TokenValue)
import Wallet exposing (Wallet)


view :
    DisplayProfile
    -> Model
    -> Element Msg
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
            , subTitleEl dProfile model.now
            , bodyEl dProfile model
            , verifyJurisdictionErrorEl dProfile model.jurisdictionCheckStatus
            ]


titleEl :
    DisplayProfile
    -> Element Msg
titleEl dProfile =
    Element.el
        [ Element.Font.size <|
            responsiveVal
                dProfile
                50
                25
        , Element.Font.color EH.white
        , responsiveVal
            dProfile
            Element.Font.medium
            Element.Font.semiBold
        , Element.centerX
        ]
    <|
        Element.text "Farming for Fryers!"


subTitleEl :
    DisplayProfile
    -> Time.Posix
    -> Element Msg
subTitleEl dProfile now =
    Element.el
        [ Element.Font.size <|
            responsiveVal
                dProfile
                30
                16
        , Element.Font.color EH.white
        , Element.Font.medium
        , Element.Font.italic
        , Element.centerX
        ]
    <|
        Element.text
            ("Farming ends in "
                ++ TimeHelpers.toDetailIntervalString
                    (TimeHelpers.sub
                        (TimeHelpers.secondsToPosix Config.farmingPeriodEnds)
                        now
                    )
            )


bodyEl :
    DisplayProfile
    -> Model
    -> Element Msg
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
                model.jurisdictionCheckStatus
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
            responsiveVal
                dProfile
                (Element.px 800)
                Element.fill
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


balancesElement :
    DisplayProfile
    -> JurisdictionCheckStatus
    -> Time.Posix
    -> Wallet
    -> Maybe UserStakingInfo
    -> DepositOrWithdrawUXModel
    -> Element Msg
balancesElement dProfile jurisdictionCheckStatus now wallet maybeUserStakingInfo depositWithdrawUXModel =
    case Wallet.userInfo wallet of
        Nothing ->
            Common.View.web3ConnectButton
                dProfile
                [ Element.centerY
                , Element.centerX
                ]
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
                        , Element.height Element.fill
                        ]
                        [ maybeGetLiquidityMessageElement
                            dProfile
                            userStakingInfo
                        , unstakedRow
                            dProfile
                            jurisdictionCheckStatus
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


apyElement :
    DisplayProfile
    -> Maybe Float
    -> Element Msg
apyElement dProfile maybeApy =
    Element.el
        ([ Element.alignTop
         , Element.alignRight
         , Element.Font.size <|
            responsiveVal
                dProfile
                30
                15
         ]
            ++ (case dProfile of
                    EH.Desktop ->
                        []

                    EH.Mobile ->
                        [ Element.Font.semiBold ]
               )
        )
    <|
        case maybeApy of
            Nothing ->
                Element.el
                    [ Element.Font.italic ]
                    (Element.text "Fetching APY...")

            Just apy ->
                let
                    mainEl =
                        case dProfile of
                            EH.Desktop ->
                                Element.column

                            EH.Mobile ->
                                Element.row
                in
                mainEl
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


maybeGetLiquidityMessageElement :
    DisplayProfile
    -> UserStakingInfo
    -> Element Msg
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


unstakedRow :
    DisplayProfile
    -> JurisdictionCheckStatus
    -> UserStakingInfo
    -> DepositOrWithdrawUXModel
    -> Element Msg
unstakedRow dProfile jurisdictionCheckStatus userStakingInfo depositOrWithdrawUXModel =
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
            "Unstaked Balance"
        , unstakedRowUX
            dProfile
            jurisdictionCheckStatus
            userStakingInfo
            maybeDepositAmountUXModel
        ]


unstakedRowUX :
    DisplayProfile
    -> JurisdictionCheckStatus
    -> UserStakingInfo
    -> Maybe AmountUXModel
    -> Element Msg
unstakedRowUX dProfile jurisdictionCheckStatus stakingInfo maybeDepositAmountUXModel =
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
        (case jurisdictionCheckStatus of
            Checked JurisdictionsWeArentIntimidatedIntoExcluding ->
                [ balanceOutputOrInput
                    dProfile
                    False
                    stakingInfo.unstaked
                    maybeDepositAmountUXModel
                    "ETHFRY"
                ]
                    ++ (case maybeDepositAmountUXModel of
                            Just depositAmountUX ->
                                [ activeDepositUXButtons
                                    dProfile
                                    depositAmountUX
                                    stakingInfo.unstaked
                                ]

                            Nothing ->
                                [ inactiveUnstackedRowButtons
                                    dProfile
                                    stakingInfo
                                ]
                       )

            _ ->
                [ verifyJurisdictionButtonOrResult
                    dProfile
                    jurisdictionCheckStatus
                ]
        )


stakedRow :
    DisplayProfile
    -> UserStakingInfo
    -> DepositOrWithdrawUXModel
    -> Element Msg
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
            "Currently Staking"
        , stakedRowUX
            dProfile
            stakingInfo
            maybeWithdrawAmountUXModel
        ]


stakedRowUX :
    DisplayProfile
    -> UserStakingInfo
    -> Maybe AmountUXModel
    -> Element Msg
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
            "ETHFRY"
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


stakedRowUXButtons :
    DisplayProfile
    -> TokenValue
    -> Element Msg
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


rewardsRow :
    DisplayProfile
    -> UserStakingInfo
    -> Time.Posix
    -> Element Msg
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


rewardsRowUX :
    DisplayProfile
    -> UserStakingInfo
    -> Time.Posix
    -> Element Msg
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
                dProfile
        ]


rowUXStyles :
    DisplayProfile
    -> Bool
    -> List (Element.Attribute Msg)
rowUXStyles dProfile isInput =
    [ Element.spacing <|
        responsiveVal
            dProfile
            30
            15
    , Element.width Element.fill
    , Element.padding 5
    , Element.height <|
        Element.px <|
            responsiveVal
                dProfile
                50
                30
    ]
        ++ (if isInput then
                [ Element.Border.rounded 5
                , Element.Background.color <| Element.rgba 1 1 1 0.3
                ]

            else
                []
           )


balanceOutputOrInput :
    DisplayProfile
    -> Bool
    -> TokenValue
    -> Maybe AmountUXModel
    -> String
    -> Element Msg
balanceOutputOrInput dProfile isRed balance maybeAmountUXModel tokenLabel =
    let
        amountElWidth =
            responsiveVal
                dProfile
                150
                75
    in
    Element.row
        [ Element.spacing <|
            responsiveVal
                dProfile
                10
                6
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
                        [ Element.width <|
                            Element.px amountElWidth
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
                    [ Element.width <|
                        Element.px amountElWidth
                    , Element.clip
                    ]
                    (Element.text <|
                        TokenValue.toFloatString Nothing <|
                            balance
                    )
        , Element.el
            [ Element.width <|
                responsiveVal
                    dProfile
                    (Element.px 100)
                    (Element.fillPortion 1)
            , Element.Font.color
                (if isRed then
                    Element.rgb 1 0 0

                 else
                    EH.black
                )
            , Element.Font.semiBold
            ]
          <|
            Element.text tokenLabel
        ]


activeWithdrawUXButtons :
    DisplayProfile
    -> AmountUXModel
    -> TokenValue
    -> Element Msg
activeWithdrawUXButtons dProfile amountUXModel stakedBalance =
    let
        withdrawButton =
            case validateInput amountUXModel.amountInput stakedBalance of
                Just amount ->
                    makeWithdrawButton
                        dProfile
                        (Just <|
                            "Withdraw "
                                ++ TokenValue.toConciseString amount
                                ++ " ETHFRY"
                        )
                        (Just <| DoWithdraw amount)

                Nothing ->
                    makeWithdrawButton
                        dProfile
                        Nothing
                        Nothing
    in
    buttonsRow
        dProfile
        [ withdrawButton
        , uxBackButton
            dProfile
        ]


activeDepositUXButtons :
    DisplayProfile
    -> AmountUXModel
    -> TokenValue
    -> Element Msg
activeDepositUXButtons dProfile amountUXModel unstakedBalance =
    let
        depositButton =
            case validateInput amountUXModel.amountInput unstakedBalance of
                Just amount ->
                    makeDepositButton
                        dProfile
                        (Just <|
                            "Deposit "
                                ++ TokenValue.toConciseString amount
                                ++ " ETHFRY"
                        )
                        (Just <| DoDeposit amount)

                Nothing ->
                    makeDepositButton
                        dProfile
                        Nothing
                        Nothing
    in
    buttonsRow
        dProfile
        [ depositButton
        , uxBackButton
            dProfile
        ]


buttonsRow :
    DisplayProfile
    -> List (Element Msg)
    -> Element Msg
buttonsRow dProfile =
    Element.row
        [ Element.spacing <|
            responsiveVal
                dProfile
                10
                5
        ]


maybeStartWithdrawButton :
    DisplayProfile
    -> TokenValue
    -> Element Msg
maybeStartWithdrawButton dProfile currentBalance =
    if TokenValue.isZero currentBalance then
        Element.none

    else
        makeWithdrawButton
            dProfile
            (Just "Withdraw ETHFRY")
            (Just <| StartWithdraw currentBalance)


maybeExitButton :
    DisplayProfile
    -> TokenValue
    -> Element Msg
maybeExitButton dProfile stakedAmount =
    if TokenValue.isZero stakedAmount then
        Element.none

    else
        exitButton
            dProfile


inactiveUnstackedRowButtons :
    DisplayProfile
    -> UserStakingInfo
    -> Element Msg
inactiveUnstackedRowButtons dProfile stakingInfo =
    if TokenValue.isZero stakingInfo.unstaked then
        Element.none

    else if TokenValue.isZero stakingInfo.allowance then
        unlockButton
            dProfile

    else
        makeDepositButton
            dProfile
            (Just "Deposit ETHFRY")
            (Just <| StartDeposit stakingInfo.unstaked)


mainRow :
    DisplayProfile
    -> List (Element Msg)
    -> Element Msg
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
        , Element.Font.size <|
            responsiveVal
                dProfile
                30
                24
        ]


rowLabel :
    DisplayProfile
    -> String
    -> Element Msg
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


commonImageAttributes : DisplayProfile -> List (Attribute msg)
commonImageAttributes dProfile =
    [ Element.centerX
    , Element.centerY
    , Element.width <|
        Element.px <|
            responsiveVal
                dProfile
                40
                25
    ]


unlockButton :
    DisplayProfile
    -> Element Msg
unlockButton dProfile =
    Element.el
        (actionButtonStyles
            dProfile
            (Just "Approve ETHFRY for Deposit")
            (Just DoUnlock)
        )
    <|
        Images.toElement
            (commonImageAttributes
                dProfile
            )
            Images.unlock


makeDepositButton :
    DisplayProfile
    -> Maybe String
    -> Maybe Msg
    -> Element Msg
makeDepositButton dProfile maybeHoverText maybeOnClick =
    Element.el
        (actionButtonStyles
            dProfile
            maybeHoverText
            maybeOnClick
        )
    <|
        Images.toElement
            (commonImageAttributes
                dProfile
            )
            Images.stakingDeposit


makeWithdrawButton :
    DisplayProfile
    -> Maybe String
    -> Maybe Msg
    -> Element Msg
makeWithdrawButton dProfile maybeHoverText maybeOnClick =
    Element.el
        (actionButtonStyles
            dProfile
            maybeHoverText
            maybeOnClick
        )
    <|
        Images.toElement
            (commonImageAttributes
                dProfile
            )
            Images.stakingWithdraw


exitButton :
    DisplayProfile
    -> Element Msg
exitButton dProfile =
    Element.el
        (actionButtonStyles
            dProfile
            (Just "Exit with all assets (FRY and ETHFRY)")
            (Just DoExit)
        )
    <|
        Images.toElement
            (commonImageAttributes
                dProfile
            )
            Images.stakingExit


claimRewardsButton :
    DisplayProfile
    -> Element Msg
claimRewardsButton dProfile =
    Element.el
        (actionButtonStyles
            dProfile
            (Just "Claim FRY Rewards")
            (Just DoClaimRewards)
        )
    <|
        Images.toElement
            (commonImageAttributes
                dProfile
            )
            Images.stakingClaimReward


uxBackButton :
    DisplayProfile
    -> Element Msg
uxBackButton dProfile =
    Element.el
        (actionButtonStyles
            dProfile
            (Just "Back")
            (Just UXBack)
        )
    <|
        Images.toElement
            (commonImageAttributes
                dProfile
            )
            Images.back


actionButtonStyles :
    DisplayProfile
    -> Maybe String
    -> Maybe Msg
    -> List (Element.Attribute Msg)
actionButtonStyles dProfile maybeHoverText maybeOnClick =
    [ Element.width <|
        Element.px <|
            responsiveVal
                dProfile
                45
                35
    , Element.height <|
        Element.px <|
            responsiveVal
                dProfile
                45
                35
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


msgInsteadOfButton :
    DisplayProfile
    -> String
    -> Element.Color
    -> Element Msg
msgInsteadOfButton dProfile text color =
    Element.el
        [ Element.centerX
        , Element.Font.size <| responsiveVal dProfile 18 12
        , Element.Font.italic
        , Element.Font.color color
        ]
        (Element.text text)


verifyJurisdictionButtonOrResult :
    DisplayProfile
    -> JurisdictionCheckStatus
    -> Element Msg
verifyJurisdictionButtonOrResult dProfile jurisdictionCheckStatus =
    case jurisdictionCheckStatus of
        WaitingForClick ->
            EH.redButton
                dProfile
                [ Element.width Element.fill ]
                [ "Confirm you are not a US citizen" ]
                VerifyJurisdictionClicked

        Checking ->
            EH.disabledButton
                dProfile
                [ Element.width Element.fill ]
                "Verifying Jurisdiction..."
                Nothing

        Error errStr ->
            Element.column
                [ Element.spacing 10
                , Element.width Element.fill
                ]
                [ msgInsteadOfButton dProfile "Error verifying jurisdiction." EH.red

                -- , Element.paragraph
                --     [ Element.Font.color EH.grayTextColor ]
                --     [ Element.text errStr ]
                -- , Element.paragraph
                --     [ Element.Font.color EH.grayTextColor ]
                --     [ Element.text "There may be more info in the console." ]
                ]

        Checked ForbiddenJurisdictions ->
            msgInsteadOfButton dProfile "Sorry, US citizens and residents are excluded." EH.red

        Checked JurisdictionsWeArentIntimidatedIntoExcluding ->
            msgInsteadOfButton dProfile "Jurisdiction Verified." EH.green


verifyJurisdictionErrorEl :
    DisplayProfile
    -> JurisdictionCheckStatus
    -> Element Msg
verifyJurisdictionErrorEl dProfile jurisdictionCheckStatus =
    case jurisdictionCheckStatus of
        Error errStr ->
            Element.column
                [ Element.spacing 20
                , Element.width Element.fill
                ]
                [ Element.paragraph
                    [ Element.Font.color EH.white
                    ]
                    [ Element.text errStr ]
                , Element.paragraph
                    [ Element.Font.color EH.white
                    ]
                    [ Element.text "There may be more info in the console." ]
                ]

        _ ->
            Element.none
