module View.Farm exposing (..)

import Config
import Element exposing (Attribute, Element, el, text)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import ElementHelpers as EH exposing (DisplayProfile, responsiveVal)
import FormatFloat
import Helpers.Time as TimeHelpers
import Images
import Maybe.Extra
import Misc exposing (calcAvailableRewards, calcTimeLeft, userInfo, validateInput)
import Theme
import Time
import TokenValue exposing (TokenValue)
import Types exposing (AmountUXModel, DepositOrWithdraw(..), DepositOrWithdrawUXModel, JurisdictionCheckStatus, Model, Msg, UserStakingInfo, Wallet)
import View.Common


view : Model -> Element Msg
view model =
    let
        dProfile =
            model.dProfile
    in
    el
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
            , Element.height Element.fill
            ]
            [ titleEl dProfile
            , subTitleEl dProfile model.now
            , farmVideoEl dProfile
            , bodyEl model
            , verifyJurisdictionErrorEl
                dProfile
                model.jurisdictionCheckStatus
                [ Element.Font.color EH.white ]
            ]


titleEl : DisplayProfile -> Element Msg
titleEl dProfile =
    text "Farming for Fryers!"
        |> el
            [ Element.Font.size <|
                responsiveVal
                    dProfile
                    50
                    25
            , Element.Font.color EH.white
            , responsiveVal
                dProfile
                Element.Font.bold
                Element.Font.semiBold
            , Element.centerX
            ]


subTitleEl : DisplayProfile -> Time.Posix -> Element Msg
subTitleEl dProfile now =
    (if calcTimeLeft now <= 0 then
        "Farming has ended!"

     else
        "Farming ends in "
            ++ TimeHelpers.toConciseIntervalString
                (TimeHelpers.sub
                    (TimeHelpers.secondsToPosix Config.farmingPeriodEnds)
                    now
                )
    )
        |> text
        |> el
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


farmVideoEl : DisplayProfile -> Element Msg
farmVideoEl dProfile =
    el
        [ Element.Font.size <|
            responsiveVal
                dProfile
                24
                12
        , Element.Font.color EH.white
        , Element.Font.medium
        , Element.Font.italic
        , Element.centerX
        , Element.Font.underline
        ]
    <|
        Element.newTabLink
            []
            { label = text "Click here to learn how to farm $FRY!"
            , url = "https://www.youtube.com/watch?v=AKfuuy7vUjA&ab_channel=FoundryDAO"
            }


bodyEl : Model -> Element Msg
bodyEl model =
    let
        dProfile =
            model.dProfile

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
                model.now
                model.apy
    in
    mainEl
        [ Element.centerX
        , Element.Background.color <| Element.rgba 1 1 1 0.1
        , Element.Border.rounded 10
        , Element.Border.glow EH.white 2
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
    case userInfo wallet of
        Nothing ->
            View.Common.web3ConnectButton
                dProfile
                [ Element.centerY
                , Element.centerX
                ]
                (EH.Action Types.ConnectToWeb3)

        Just userInfo ->
            case maybeUserStakingInfo of
                Nothing ->
                    el
                        [ Element.Font.italic
                        , Element.centerY
                        ]
                        (text "Fetching info...")

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
                            now
                            jurisdictionCheckStatus
                            userStakingInfo
                            depositWithdrawUXModel
                        , stakedRow
                            dProfile
                            userStakingInfo
                            depositWithdrawUXModel
                        , rewardsRow
                            dProfile
                            now
                            userStakingInfo
                        ]


apyElement :
    DisplayProfile
    -> Time.Posix
    -> Maybe Float
    -> Element Msg
apyElement dProfile now maybeApy =
    el
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
                el
                    [ Element.Font.italic ]
                    (text "Fetching APY...")

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
                    , Element.Background.color <| Element.rgba 1 1 1 0.3
                    , Element.Border.rounded 5
                    , Element.Border.glow Theme.lightGray 1
                    ]
                    [ el
                        [ Element.Font.color EH.white ]
                      <|
                        text "Current APY: "
                    , el
                        [ Element.centerX
                        , Element.Font.color EH.black
                        , Element.Font.semiBold
                        ]
                      <|
                        text <|
                            FormatFloat.formatFloat 2
                                (if calcTimeLeft now <= 0 then
                                    0

                                 else
                                    apy
                                )
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
            , Element.Background.color <| Element.rgba 1 1 1 0.9
            , Element.padding 10
            , Element.Border.rounded 5
            ]
            [ Element.newTabLink
                [ Element.Font.color Theme.blue ]
                { url = Config.urlToLiquidityPool
                , label = text "Obtain ETHFRY Liquidity"
                }
            , text " to continue."
            ]

    else
        Element.none


unstakedRow :
    DisplayProfile
    -> Time.Posix
    -> JurisdictionCheckStatus
    -> UserStakingInfo
    -> DepositOrWithdrawUXModel
    -> Element Msg
unstakedRow dProfile now jurisdictionCheckStatus userStakingInfo depositOrWithdrawUXModel =
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
        , if calcTimeLeft now <= 0 then
            text "Farming has ended"

          else
            unstakedRowUX
                dProfile
                now
                jurisdictionCheckStatus
                userStakingInfo
                maybeDepositAmountUXModel
        ]


unstakedRowUX :
    DisplayProfile
    -> Time.Posix
    -> JurisdictionCheckStatus
    -> UserStakingInfo
    -> Maybe AmountUXModel
    -> Element Msg
unstakedRowUX dProfile now jurisdictionCheckStatus stakingInfo maybeDepositAmountUXModel =
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
            Types.Checked Types.JurisdictionsWeArentIntimidatedIntoExcluding ->
                [ balanceOutputOrInput
                    dProfile
                    EH.black
                    stakingInfo.unstaked
                    maybeDepositAmountUXModel
                    "ETHFRY"
                ]
                    ++ (if calcTimeLeft now > 0 then
                            case maybeDepositAmountUXModel of
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

                        else
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
            rowUXStyles
                dProfile
                isInput
    in
    Element.row
        rowStyles
        [ balanceOutputOrInput
            dProfile
            EH.black
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
    -> Time.Posix
    -> UserStakingInfo
    -> Element Msg
rewardsRow dProfile now stakingInfo =
    mainRow
        dProfile
        [ rowLabel
            dProfile
            "Available Rewards"
        , rewardsRowUX
            dProfile
            now
            stakingInfo
        ]


rewardsRowUX :
    DisplayProfile
    -> Time.Posix
    -> UserStakingInfo
    -> Element Msg
rewardsRowUX dProfile now stakingInfo =
    let
        availableRewards =
            calcAvailableRewards stakingInfo now
    in
    Element.row
        (rowUXStyles
            dProfile
            False
        )
        [ balanceOutputOrInput
            dProfile
            EH.black
            availableRewards
            Nothing
            "FRY"
        , if TokenValue.isZero <| availableRewards then
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
    , Element.Font.color EH.white
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
    -> Element.Color
    -> TokenValue
    -> Maybe AmountUXModel
    -> String
    -> Element Msg
balanceOutputOrInput dProfile color balance maybeAmountUXModel tokenLabel =
    let
        amountElWidth =
            responsiveVal
                dProfile
                150
                75
    in
    Element.row
        [ Element.spacing <| responsiveVal dProfile 10 6
        , Element.Font.size <| responsiveVal dProfile 26 16
        , Element.Background.color <| Element.rgba 1 1 1 0.2
        , Element.Font.color EH.black
        , Element.padding 5
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
                    { onChange = Types.AmountInputChanged
                    , text = amountUXModel.amountInput
                    , placeholder = Nothing
                    , label = Element.Input.labelHidden "amount"
                    }

            Nothing ->
                el
                    [ Element.width <|
                        Element.px amountElWidth
                    , Element.clip
                    ]
                    (text <|
                        TokenValue.toFloatString Nothing <|
                            balance
                    )
        , el
            [ Element.width <|
                responsiveVal
                    dProfile
                    (Element.px 100)
                    (Element.fillPortion 1)
            , Element.Font.color
                color
            , Element.Font.semiBold
            , Element.alignRight
            ]
          <|
            Element.row
                [ Element.alignRight ]
                [ text tokenLabel ]
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
                        (Just <| Types.DoWithdraw amount)

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
                        (Just <| Types.DoDeposit amount)

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
        [ Element.spacing <| responsiveVal dProfile 10 5
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
            (Just <| Types.StartWithdraw currentBalance)


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
            (Just <| Types.StartDeposit stakingInfo.unstaked)


mainRow :
    DisplayProfile
    -> List (Element Msg)
    -> Element Msg
mainRow dProfile =
    Element.column
        [ Element.spacing 5
        , Element.Background.color <| Element.rgba 1 1 1 0.15
        , Element.padding 10
        , Element.Border.rounded 5
        , Element.Border.glow Theme.lightGray 1
        , Element.Font.size <| responsiveVal dProfile 30 24
        , Element.width <|
            responsiveVal
                dProfile
                (Element.px 420)
                Element.fill
        ]


rowLabel :
    DisplayProfile
    -> String
    -> Element Msg
rowLabel dProfile textToDisplay =
    text textToDisplay
        |> el
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
            , Element.Font.regular
            , Element.Font.color EH.white
            ]


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
    el
        (actionButtonStyles
            dProfile
            (Just "Approve ETHFRY for Deposit")
            (Just Types.DoUnlock)
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
    el
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
    el
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
    el
        (actionButtonStyles
            dProfile
            (Just "Exit with all assets (FRY and ETHFRY)")
            (Just Types.DoExit)
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
    el
        (actionButtonStyles
            dProfile
            (Just "Claim FRY Rewards")
            (Just Types.DoClaimRewards)
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
    el
        (actionButtonStyles
            dProfile
            (Just "Back")
            (Just Types.UXBack)
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
            [ Maybe.map EH.withHovertext maybeHoverText ]
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
msgInsteadOfButton dProfile textToDisplay color =
    text textToDisplay
        |> el
            [ Element.centerX
            , Element.Font.size <|
                responsiveVal
                    dProfile
                    18
                    12
            , Element.Font.italic
            , Element.Font.color color
            ]


verifyJurisdictionButtonOrResult :
    DisplayProfile
    -> JurisdictionCheckStatus
    -> Element Msg
verifyJurisdictionButtonOrResult dProfile jurisdictionCheckStatus =
    case jurisdictionCheckStatus of
        Types.WaitingForClick ->
            Theme.redButton
                dProfile
                [ Element.width Element.fill ]
                [ "Confirm you are not a US citizen" ]
                (EH.Action Types.VerifyJurisdictionClicked)

        Types.Checking ->
            Theme.disabledButton
                dProfile
                [ Element.width Element.fill
                , Element.Font.color EH.black
                ]
                "Verifying Jurisdiction..."
                Nothing

        Types.Error errStr ->
            Element.column
                [ Element.spacing 10
                , Element.width Element.fill
                ]
                [ msgInsteadOfButton
                    dProfile
                    "Error verifying jurisdiction."
                    Theme.red
                ]

        Types.Checked Types.ForbiddenJurisdictions ->
            msgInsteadOfButton
                dProfile
                "Sorry, US citizens and residents are excluded."
                Theme.red

        Types.Checked Types.JurisdictionsWeArentIntimidatedIntoExcluding ->
            msgInsteadOfButton
                dProfile
                "Jurisdiction Verified."
                Theme.green


verifyJurisdictionErrorEl :
    DisplayProfile
    -> JurisdictionCheckStatus
    -> List (Attribute Msg)
    -> Element Msg
verifyJurisdictionErrorEl dProfile jurisdictionCheckStatus attributes =
    case jurisdictionCheckStatus of
        Types.Error errStr ->
            Element.column
                ([ Element.spacing <|
                    responsiveVal
                        dProfile
                        20
                        10
                 , Element.Font.size <|
                    responsiveVal
                        dProfile
                        16
                        10
                 ]
                    ++ attributes
                )
                [ el
                    []
                  <|
                    text errStr
                , text "There may be more info in the console."
                ]

        _ ->
            Element.none
