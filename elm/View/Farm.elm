module View.Farm exposing (..)

import Chain exposing (whenJust)
import Config
import Element exposing (Attribute, Element, alignRight, alignTop, centerX, centerY, column, el, fill, fillPortion, height, minimum, padding, paddingEach, px, rgba, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import ElementHelpers as EH exposing (DisplayProfile, black, responsiveVal)
import FormatFloat
import Helpers.Time as TimeHelpers
import Images
import Maybe.Extra
import Misc exposing (calcAvailableRewards, calcTimeLeft, userInfo, validateInput)
import Theme
import Time
import TokenValue exposing (TokenValue)
import Types exposing (AmountUXModel, Chain, DepositOrWithdraw(..), DepositOrWithdrawUXModel, JurisdictionCheckStatus, Model, Msg(..), UserInfo, UserStakingInfo, Wallet)
import View.Attrs exposing (hover)
import View.Common
import View.Img
import Wallet


view : Model -> Element Msg
view model =
    let
        dProfile =
            model.dProfile
    in
    (if not model.farmingIsActive then
        [ titleEl dProfile "Farming has ended for now!" ]

     else
        [ titleEl dProfile "Farming for Fryers!"
        , subTitleEl dProfile model.now

        --, farmVideoEl dProfile
        , if model.chainSwitchInProgress then
            View.Img.spinner 20 Theme.softRed

          else
            bodyEl model
        , verifyJurisdictionErrorEl
            dProfile
            model.jurisdictionCheckStatus
            [ Font.color EH.white ]
        ]
    )
        |> column
            [ centerX
            , spacing <| responsiveVal dProfile 40 15
            , height fill
            ]
        |> el
            [ width fill
            , paddingEach
                { top = responsiveVal dProfile 60 15
                , bottom = 0
                , left = 0
                , right = 0
                }
            ]


titleEl : DisplayProfile -> String -> Element Msg
titleEl dProfile title =
    text title
        |> el
            [ Font.size <| responsiveVal dProfile 50 25
            , Font.color EH.white
            , responsiveVal
                dProfile
                Font.bold
                Font.semiBold
            , centerX
            ]


subTitleEl : DisplayProfile -> Time.Posix -> Element Msg
subTitleEl dProfile now =
    "Farming ends in "
        ++ TimeHelpers.toConciseIntervalString
            (TimeHelpers.sub
                (TimeHelpers.secondsToPosix Config.farmingPeriodEnds)
                now
            )
        |> text
        |> el
            [ Font.size <| responsiveVal dProfile 30 16
            , Font.color EH.white
            , Font.medium
            , Font.italic
            , centerX
            ]


farmVideoEl : DisplayProfile -> Element Msg
farmVideoEl dProfile =
    el
        [ Font.size <| responsiveVal dProfile 24 12
        , Font.color EH.white
        , Font.medium
        , Font.italic
        , centerX
        , Font.underline
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
                    row

                EH.Mobile ->
                    column

        chain =
            model.wallet
                |> Wallet.userInfo
                |> whenJust
                    (\userInfo ->
                        userInfo.chain
                    )

        balancesEl =
            balancesElement
                chain
                dProfile
                model.jurisdictionCheckStatus
                model.now
                model.farmingIsActive
                model.wallet
                model.userStakingInfo
                model.depositWithdrawUXModel

        apyEl =
            apyElement
                dProfile
                model.apy
    in
    mainEl
        ([ centerX
         , height <| px <| responsiveVal dProfile 500 400
         , width <| responsiveVal dProfile (px 800) (fill |> minimum 300)
         , padding <| responsiveVal dProfile 20 10
         , spacing <| responsiveVal dProfile 10 5
         ]
            ++ Theme.mainContainerBackgroundAttributes
            ++ Theme.mainContainerBorderAttributes
        )
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
    Chain
    -> DisplayProfile
    -> JurisdictionCheckStatus
    -> Time.Posix
    -> Bool
    -> Wallet
    -> Maybe UserStakingInfo
    -> DepositOrWithdrawUXModel
    -> Element Msg
balancesElement chain dProfile jurisdictionCheckStatus now isFarmingActive wallet maybeUserStakingInfo depositWithdrawUXModel =
    case userInfo wallet of
        Nothing ->
            View.Common.web3ConnectButton
                dProfile
                [ centerY
                , centerX
                ]
                (EH.Action Types.ConnectToWeb3)

        Just userInfo ->
            case maybeUserStakingInfo of
                Nothing ->
                    text "Fetching info..."
                        |> el
                            [ Font.italic
                            , centerY
                            , Font.color EH.white
                            ]

                Just userStakingInfo ->
                    [ maybeGetLiquidityMessageElement
                        chain
                        dProfile
                        userStakingInfo
                    , unstakedRow
                        dProfile
                        now
                        isFarmingActive
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
                        |> column
                            [ spacing <| responsiveVal dProfile 25 15
                            , alignTop
                            , height fill
                            ]


apyElement :
    DisplayProfile
    -> Maybe Float
    -> Element Msg
apyElement dProfile maybeApy =
    (case maybeApy of
        Nothing ->
            text "Fetching APY..."
                |> el
                    [ Font.italic
                    , Font.color EH.white
                    ]

        Just apy ->
            let
                mainEl =
                    case dProfile of
                        EH.Desktop ->
                            column

                        EH.Mobile ->
                            row
            in
            [ text "Current APY: "
                |> el
                    [ Font.color EH.white ]
            , FormatFloat.formatFloat 2
                apy
                ++ "%"
                |> text
                |> el
                    [ centerX
                    , Font.color Theme.almostWhite
                    ]
            ]
                |> mainEl
                    ([ spacing 5
                     , padding 5
                     ]
                        ++ Theme.mainContainerBackgroundAttributes
                        ++ Theme.mainContainerBorderAttributes
                    )
    )
        |> el
            ([ alignTop
             , alignRight
             , Font.size <| responsiveVal dProfile 30 16
             ]
                ++ (case dProfile of
                        EH.Desktop ->
                            []

                        EH.Mobile ->
                            [ Font.semiBold
                            , Element.paddingEach { top = 0, left = 0, right = 0, bottom = 10 }
                            ]
                   )
            )


maybeGetLiquidityMessageElement : Chain -> DisplayProfile -> UserStakingInfo -> Element Msg
maybeGetLiquidityMessageElement chain dProfile stakingInfo =
    if
        TokenValue.isZero stakingInfo.staked
            && TokenValue.isZero stakingInfo.unstaked
            && TokenValue.isZero stakingInfo.claimableRewards
    then
        row
            [ centerX
            , Font.size <| responsiveVal dProfile 20 15
            , Background.color <| rgba 1 1 1 0.9
            , padding 10
            , Border.rounded 5
            ]
            [ Element.newTabLink
                [ Font.color Theme.blue ]
                { url = Config.urlToLiquidityPool chain
                , label = text "Obtain ETHFRY Liquidity"
                }
            , text " to continue."
            ]

    else
        Element.none


unstakedRow :
    DisplayProfile
    -> Time.Posix
    -> Bool
    -> JurisdictionCheckStatus
    -> UserStakingInfo
    -> DepositOrWithdrawUXModel
    -> Element Msg
unstakedRow dProfile now isFarmingActive jurisdictionCheckStatus userStakingInfo depositOrWithdrawUXModel =
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
            now
            isFarmingActive
            jurisdictionCheckStatus
            userStakingInfo
            maybeDepositAmountUXModel
        ]


unstakedRowUX :
    DisplayProfile
    -> Time.Posix
    -> Bool
    -> JurisdictionCheckStatus
    -> UserStakingInfo
    -> Maybe AmountUXModel
    -> Element Msg
unstakedRowUX dProfile now isFarmingActive jurisdictionCheckStatus stakingInfo maybeDepositAmountUXModel =
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
    row
        rowStyles
        (case jurisdictionCheckStatus of
            Types.Checked Types.JurisdictionsWeArentIntimidatedIntoExcluding ->
                [ balanceOutputOrInput
                    dProfile
                    Theme.almostWhite
                    stakingInfo.unstaked
                    maybeDepositAmountUXModel
                    "ETHFRY"
                , if isFarmingActive then
                    case maybeDepositAmountUXModel of
                        Just depositAmountUX ->
                            activeDepositUXButtons
                                dProfile
                                depositAmountUX
                                stakingInfo.unstaked

                        Nothing ->
                            inactiveUnstackedRowButtons
                                dProfile
                                stakingInfo

                  else
                    inactiveUnstackedRowButtons
                        dProfile
                        stakingInfo
                ]

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
    row
        rowStyles
        [ balanceOutputOrInput
            dProfile
            Theme.almostWhite
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
    row
        (rowUXStyles
            dProfile
            False
        )
        [ balanceOutputOrInput
            dProfile
            Theme.almostWhite
            availableRewards
            Nothing
            "DAI"
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
    [ spacing (responsiveVal dProfile 30 15)
    , width fill
    , padding 5
    , height <| px <| responsiveVal dProfile 50 30
    , Font.color EH.white
    ]
        ++ (if isInput then
                [ Border.rounded 5
                , Background.color <| rgba 1 1 1 0.3
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
            responsiveVal dProfile 150 75
    in
    row
        [ spacing <| responsiveVal dProfile 10 6
        , Font.size <| responsiveVal dProfile 26 16
        , Background.color <| rgba 1 1 1 0.2
        , Font.color Theme.almostWhite
        , padding 5
        ]
        [ case maybeAmountUXModel of
            Just amountUXModel ->
                let
                    inputStyles =
                        [ width <| px amountElWidth
                        , Background.color <| rgba 1 1 1 0.3
                        , padding 0
                        , Border.width 0
                        , Font.color EH.white
                        ]
                            ++ (if validateInput amountUXModel.amountInput balance == Nothing then
                                    [ Border.width 2
                                    , Border.color <| Theme.darkRed
                                    ]

                                else
                                    []
                               )
                in
                Input.text
                    inputStyles
                    { onChange = Types.AmountInputChanged
                    , text = amountUXModel.amountInput
                    , placeholder = Nothing
                    , label = Input.labelHidden "amount"
                    }

            Nothing ->
                el
                    [ width <| px amountElWidth
                    , Element.clip
                    ]
                    (balance |> TokenValue.toFloatString Nothing |> text)
        , el
            [ width <| responsiveVal dProfile (px 100) (fillPortion 1)
            , Font.color color
            , Font.semiBold
            , alignRight
            ]
          <|
            row
                [ alignRight ]
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
    row
        [ spacing <| responsiveVal dProfile 8 5
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
    column
        ([ spacing 5
         , padding 10
         , Font.size <| responsiveVal dProfile 30 24
         , width <| responsiveVal dProfile (px 420) fill
         ]
            ++ Theme.mainContainerBorderAttributes
            ++ Theme.mainContainerBackgroundAttributes
        )


rowLabel :
    DisplayProfile
    -> String
    -> Element Msg
rowLabel dProfile textToDisplay =
    text textToDisplay
        |> el
            [ width <| responsiveVal dProfile (px 280) fill
            , Font.size <| responsiveVal dProfile 40 20
            , Font.regular
            , Font.color EH.white
            ]


commonImageAttributes : DisplayProfile -> List (Attribute msg)
commonImageAttributes dProfile =
    [ centerX
    , centerY
    , width <| px <| responsiveVal dProfile 40 25
    ]


unlockButton :
    DisplayProfile
    -> Element Msg
unlockButton dProfile =
    Images.unlock
        |> Images.toElement
            (commonImageAttributes dProfile)
        |> el
            (actionButtonStyles
                dProfile
                (Just "Approve ETHFRY for Deposit")
                (Just Types.DoUnlock)
            )


makeDepositButton :
    DisplayProfile
    -> Maybe String
    -> Maybe Msg
    -> Element Msg
makeDepositButton dProfile maybeHoverText maybeOnClick =
    Images.stakingDeposit
        |> Images.toElement
            (commonImageAttributes
                dProfile
            )
        |> el
            (actionButtonStyles
                dProfile
                maybeHoverText
                maybeOnClick
            )


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
            (Just "Exit with all assets (DAI and ETHFRY)")
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
            (Just "Claim DAI Rewards")
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
    Images.back
        |> Images.toElement
            (commonImageAttributes
                dProfile
            )
        |> el
            (actionButtonStyles
                dProfile
                (Just "Back")
                (Just Types.UXBack)
            )


actionButtonStyles :
    DisplayProfile
    -> Maybe String
    -> Maybe Msg
    -> List (Element.Attribute Msg)
actionButtonStyles dProfile maybeHoverText maybeOnClick =
    [ width <| px <| responsiveVal dProfile 45 35
    , height <| px <| responsiveVal dProfile 45 35
    , Border.rounded 6
    , Border.width 1
    , Border.color <| rgba 0 0 0 0.2
    ]
        ++ Maybe.Extra.values
            [ Maybe.map EH.withHovertext maybeHoverText ]
        ++ (case maybeOnClick of
                Just onClick ->
                    [ Element.pointer
                    , Events.onClick onClick
                    , Background.color <| rgba 1 1 1 0.2
                    ]

                Nothing ->
                    [ Background.color <| Element.rgb 0.7 0.7 0.7 ]
           )


msgInsteadOfButton :
    DisplayProfile
    -> String
    -> Element.Color
    -> Element Msg
msgInsteadOfButton dProfile textToDisplay color =
    text textToDisplay
        |> el
            [ centerX
            , Font.size <| responsiveVal dProfile 18 12
            , Font.italic
            , Font.color color
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
                [ width fill ]
                [ "Confirm you are not a US citizen" ]
                (EH.Action Types.VerifyJurisdictionClicked)

        Types.Checking ->
            Theme.disabledButton
                dProfile
                [ width fill
                , Font.color EH.black
                ]
                "Verifying Jurisdiction..."
                Nothing

        Types.Error errStr ->
            column
                [ spacing 10
                , width fill
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
            column
                ([ spacing <| responsiveVal dProfile 20 10
                 , Font.size <| responsiveVal dProfile 16 10
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


viewInstructions : Model -> UserInfo -> Element Msg
viewInstructions model userInfo =
    case userInfo.chain of
        Types.Eth ->
            [ Input.button
                [ Background.color Theme.green
                , padding 10
                , View.Attrs.roundBorder
                , hover
                , Font.color black
                , width <| px 180
                , Element.alignRight
                ]
                { onPress = Just BSCImport
                , label =
                    if model.chainSwitchInProgress then
                        View.Common.spinner 20 black
                            |> el [ centerX ]

                    else
                        text "Switch to xDai"
                            |> el [ centerX ]
                }
            ]
                |> row
                    [ width fill
                    , spacing 10
                    , padding 10
                    , Background.color Theme.orange
                    , View.Attrs.roundBorder
                    ]

        Types.XDai ->
            Element.none

        Types.BSC ->
            [ Input.button
                [ Background.color Theme.green
                , padding 10
                , View.Attrs.roundBorder
                , hover
                , Font.color black
                , width <| px 180
                , Element.alignRight
                ]
                { onPress = Just BSCImport
                , label =
                    if model.chainSwitchInProgress then
                        View.Common.spinner 20 black
                            |> el [ centerX ]

                    else
                        text "Switch to Eth Mainnet"
                            |> el [ centerX ]
                }
            ]
                |> row
                    [ width fill
                    , spacing 10
                    , padding 10
                    , Background.color Theme.orange
                    , View.Attrs.roundBorder
                    ]
