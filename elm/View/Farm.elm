module View.Farm exposing (..)

import BigInt
import Chain
import Config
import Css exposing (verticalAlign)
import Element exposing (Attribute, Element, alignRight, alignTop, centerX, centerY, column, el, fill, fillPortion, height, minimum, padding, paddingEach, px, rgba, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import ElementHelpers as EH exposing (DisplayProfile(..), black, responsiveVal, white)
import FormatFloat
import Helpers.Time as TimeHelpers
import Images
import Maybe.Extra
import Misc exposing (calcAvailableRewards, calcTimeLeft, loadingText, userInfo, validateInput)
import Theme exposing (almostWhite, blueButton, lightGray)
import Time
import TokenValue exposing (TokenValue)
import Types exposing (AmountUXModel, Chain(..), DepositOrWithdraw(..), DepositOrWithdrawUXModel, JurisdictionCheckStatus, Model, Msg(..), UserInfo, UserStakingInfo, Wallet)
import View.Attrs exposing (hover)
import View.Common
import View.Img
import Wallet


view : Model -> Element Msg
view model =
    let
        dProfile =
            model.dProfile

        chain =
            model.wallet
                |> Wallet.getChainDefaultEth
    in
    (case chain of
        Eth ->
            [ titleEl dProfile "Farming for Fryers!"
            , subTitleEl dProfile model.now model.farmingPeriodEnds
            , farmVideoEl dProfile
            , if model.chainSwitchInProgress then
                loadingText |> text |> el [ Font.color almostWhite ]

              else
                bodyEl model
            ]

        _ ->
            [ titleEl dProfile "Farming currently only available on mainnet." ]
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


subTitleEl : DisplayProfile -> Time.Posix -> Int -> Element Msg
subTitleEl dProfile now farmingPeriodEnds =
    (if farmingPeriodEnds /= 0 then
        "Farming ends in "
            ++ TimeHelpers.toConciseIntervalString
                (TimeHelpers.sub
                    (TimeHelpers.secondsToPosix farmingPeriodEnds)
                    now
                )

     else
        loadingText
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

        isFarmingActive =
            calcTimeLeft model.now model.farmingPeriodEnds /= 0

        balancesEl =
            balancesElement
                dProfile
                model.jurisdictionCheckStatus
                model.now
                isFarmingActive
                model.wallet
                model.userStakingInfo
                model.oldUserStakingBalance
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
                , [ apyEl
                  ]
                    |> column
                        [ width fill
                        , alignRight
                        , alignTop
                        , spacing 10
                        ]
                ]

            EH.Mobile ->
                [ [ [ apyEl
                    ]
                        |> row
                            [ width fill
                            , spacing 10
                            ]
                  ]
                    |> column
                        []
                , balancesEl
                ]
        )


balancesElement :
    DisplayProfile
    -> JurisdictionCheckStatus
    -> Time.Posix
    -> Bool
    -> Wallet
    -> Maybe UserStakingInfo
    -> Maybe TokenValue
    -> DepositOrWithdrawUXModel
    -> Element Msg
balancesElement dProfile jurisdictionCheckStatus now isFarmingActive wallet maybeUserStakingInfo maybeOldUserStakingBalance depositWithdrawUXModel =
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
                    [ maybeExitOldFarmElement
                        dProfile
                        maybeOldUserStakingBalance
                    , maybeGetLiquidityMessageElement
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
                            column
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
                    [ spacing 5
                    , padding 5
                    ]
    )
        |> el
            ([ alignTop
             , Font.size <| responsiveVal dProfile 30 16
             ]
                ++ (case dProfile of
                        EH.Desktop ->
                            [ alignRight
                            ]

                        EH.Mobile ->
                            [ Font.semiBold
                            , Element.paddingEach { top = 0, left = 0, right = 0, bottom = 10 }
                            ]
                   )
            )


maybeExitOldFarmElement : DisplayProfile -> Maybe TokenValue -> Element Msg
maybeExitOldFarmElement dProfile maybeOldStakingBalance =
    let
        hasOldStakingBalance =
            maybeOldStakingBalance
                |> Maybe.withDefault TokenValue.zero
                |> TokenValue.isZero
                |> not
    in
    if hasOldStakingBalance then
        Input.button
            [ Font.size <| responsiveVal dProfile 20 15
            , Font.color white
            , Background.color <| Element.rgb 1 0 0
            , padding 15
            , Border.rounded 10
            ]
            { onPress = Just DoExitFromOldFarm
            , label = Element.text "Exit from old farm"
            }

    else
        Element.none


maybeGetLiquidityMessageElement : DisplayProfile -> UserStakingInfo -> Element Msg
maybeGetLiquidityMessageElement dProfile stakingInfo =
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
                { url = Config.liquidityPoolUrl
                , label =
                    text <|
                        "Obtain liquidity tokens to continue."
                }
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
        [ balanceOutputOrInput
            dProfile
            Theme.almostWhite
            stakingInfo.unstaked
            maybeDepositAmountUXModel
            liquidityDescription
        , if isFarmingActive then
            case maybeDepositAmountUXModel of
                Just depositAmountUX ->
                    activeDepositUXButtons
                        dProfile
                        depositAmountUX
                        stakingInfo.unstaked

                Nothing ->
                    inactiveUnstakedRowButtons
                        dProfile
                        stakingInfo

          else
            inactiveUnstakedRowButtons
                dProfile
                stakingInfo
        ]


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
            liquidityDescription
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
                                ++ " "
                                ++ liquidityDescription
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
                                ++ " "
                                ++ liquidityDescription
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
            (Just <| "Withdraw " ++ liquidityDescription)
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


inactiveUnstakedRowButtons :
    DisplayProfile
    -> UserStakingInfo
    -> Element Msg
inactiveUnstakedRowButtons dProfile stakingInfo =
    if TokenValue.isZero stakingInfo.unstaked then
        Element.none

    else if TokenValue.isZero stakingInfo.allowance then
        unlockButton
            dProfile

    else
        makeDepositButton
            dProfile
            (Just <| "Deposit " ++ liquidityDescription)
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
                (Just <| "Approve " ++ liquidityDescription ++ " for Deposit")
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
            (Just <| "Exit with all assets (DAI and " ++ liquidityDescription ++ ")")
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


liquidityDescription : String
liquidityDescription =
    "BPT"
