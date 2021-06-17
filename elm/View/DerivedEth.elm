module View.DerivedEth exposing (view)

import BigInt
import Chain
import Element exposing (Attribute, Color, Element, alignRight, alignTop, centerX, column, el, fill, height, maximum, minimum, padding, paddingEach, paragraph, px, row, spacing, text, width)
import Element.Background
import Element.Border
import Element.Font as Font
import Element.Input
import ElementHelpers as EH exposing (DisplayProfile(..), responsiveVal)
import Misc exposing (userInfo)
import Theme exposing (disabledButton, green, red, redButton)
import TokenValue exposing (TokenValue)
import Types exposing (Chain(..), InputValidationResult, JurisdictionCheckStatus, Model, Msg, UserDerivedEthInfo, UserInfo)
import View.Common exposing (..)
import Wallet


view : Model -> Element Msg
view model =
    let
        dProfile =
            model.dProfile

        chain =
            model.wallet
                |> Wallet.userInfo
                |> Chain.whenJust
                    (\userinfo ->
                        userinfo.chain
                    )
    in
    [ titleEl dProfile
    , case chain of
        Eth ->
            mainEl
                dProfile
                model.depositAmount
                model.withDrawalAmount
                (userInfo model.wallet)
                model.userDerivedEthInfo

        _ ->
            "dETH currently only available on mainnet."
                |> text
                |> el
                    [ Font.size <| responsiveVal dProfile 30 16
                    , Font.color EH.white
                    , Font.medium
                    , Font.italic
                    , centerX
                    ]
    ]
        |> column
            [ padding 20
            , spacing (responsiveVal dProfile 25 10)
            , Font.color EH.white
            , width fill
            ]
        |> el
            [ Element.paddingEach
                { top =
                    responsiveVal
                        dProfile
                        60
                        10
                , bottom = 0
                , left = 0
                , right = 0
                }
            , centerX
            , alignTop
            ]


titleEl : DisplayProfile -> Element Msg
titleEl dProfile =
    [ text "\"Prepare to have your face ripped off by the Dragon.\" - dETH" ]
        |> paragraph
            [ responsiveVal
                dProfile
                50
                25
                |> Font.size
            , Font.color EH.white
            , responsiveVal
                dProfile
                Font.bold
                Font.semiBold
            , centerX
            , padding 20
            ]


mainEl : DisplayProfile -> String -> String -> Maybe UserInfo -> Maybe UserDerivedEthInfo -> Element Msg
mainEl dProfile depositAmount withdrawalAmount maybeUserInfo maybeUserDerivedEthInfo =
    (case maybeUserInfo of
        Nothing ->
            [ web3ConnectButton
                dProfile
                ([]
                    ++ (case dProfile of
                            Desktop ->
                                []

                            Mobile ->
                                [ Font.size 10
                                , Element.padding 5
                                ]
                       )
                )
                (EH.Action Types.ConnectToWeb3)
            ]

        Just userInfo ->
            case maybeUserDerivedEthInfo of
                Nothing ->
                    [ text "Loading user info..." ]

                Just userDerivedEthInfo ->
                    [ investOrWithdrawEl
                        dProfile
                        "ETH -> dETH"
                        "Deposit"
                        depositAmount
                        "ETH"
                        Types.DepositAmountChanged
                        userDerivedEthInfo
                    , investOrWithdrawEl
                        dProfile
                        "dETH -> ETH"
                        "Redeem"
                        withdrawalAmount
                        "dETH"
                        Types.WithdrawalAmountChanged
                        userDerivedEthInfo
                    ]
    )
        |> responsiveVal dProfile
            row
            column
            [ spacing 20
            , padding 20
            , centerX
            ]


investOrWithdrawEl :
    DisplayProfile
    -> String
    -> String
    -> String
    -> String
    -> (String -> Msg)
    -> UserDerivedEthInfo
    -> Element Msg
investOrWithdrawEl dProfile heading buttonText inputAmount tokenName msg userDEthInfo =
    let
        textFontSize =
            Font.size (responsiveVal dProfile 22 16)

        headingFontSize =
            Font.size (responsiveVal dProfile 28 18)

        ( amountChangedMsg, clickedMsg ) =
            if tokenName == "ETH" then
                ( Types.DepositAmountChanged, Types.DepositClicked )

            else
                ( Types.WithdrawalAmountChanged, Types.WithdrawClicked )

        msgAmount =
            case TokenValue.fromString inputAmount of
                Nothing ->
                    TokenValue.zero

                Just val ->
                    val

        userBalance =
            if tokenName == "ETH" then
                userDEthInfo.ethBalance

            else
                userDEthInfo.dEthBalance

        inputValid =
            validateInput inputAmount userBalance

        blockHeightMin =
            responsiveVal dProfile 280 220
    in
    [ text heading
        |> el
            [ headingFontSize
            , Font.semiBold
            , centerX
            ]
    , text
        (tokenName
            ++ " balance: "
            ++ (userBalance
                    |> tokenValueToString
               )
        )
        |> el
            [ textFontSize
            , centerX
            ]
    , [ percentageButtonsEl
            dProfile
            amountChangedMsg
            userBalance
      , [ inputEl
            dProfile
            inputAmount
            userBalance
            msg
        , buttonEl
            dProfile
            []
            buttonText
            (msgAmount
                |> clickedMsg
                |> Just
            )
        ]
            |> row
                [ centerX
                , spacing 10
                ]
      ]
        |> column
            [ width fill
            , spacing 10
            , paddingEach
                { top = 10
                , left = 0
                , right = 0
                , bottom = 10
                }
            ]
    ]
        ++ (if userBalance == TokenValue.zero then
                [ Element.rgba 1 0 0 0.8
                    |> msgInsteadOfButton
                        dProfile
                        ("Your " ++ tokenName ++ " balance is zero")
                ]

            else if inputValid == Types.InputGreaterThan then
                [ Element.rgba 1 0 0 0.8
                    |> msgInsteadOfButton
                        dProfile
                        "Value too high!"
                ]

            else if inputValid == Types.InputLessThan then
                [ Element.rgba 1 0 0 0.8
                    |> msgInsteadOfButton
                        dProfile
                        "Value too low!"
                ]

            else
                []
           )
        ++ (if inputValid == Types.InputValid then
                [ depositRedeemInfoEl
                    dProfile
                    tokenName
                    userDEthInfo
                    |> el
                        [ width fill
                        , paddingEach
                            { top = 0
                            , left = responsiveVal dProfile 20 10
                            , right = responsiveVal dProfile 20 10
                            , bottom = 0
                            }
                        ]
                ]

            else
                []
           )
        |> column
            (Theme.mainContainerBorderAttributes
                ++ Theme.mainContainerBackgroundAttributes
                ++ [ spacing 15
                   , padding 12
                   , centerX
                   , width
                        (fill
                            |> minimum (responsiveVal dProfile 450 300)
                        )
                   , height
                        (fill
                            |> minimum blockHeightMin
                        )
                   ]
            )


depositRedeemInfoEl : DisplayProfile -> String -> UserDerivedEthInfo -> Element Msg
depositRedeemInfoEl dProfile tokenName userDEthInfo =
    let
        textFontSize =
            responsiveVal dProfile 16 10
                |> Font.size

        elems =
            if tokenName == "ETH" then
                [ depositRedeemInfoItemEl
                    textFontSize
                    "Actual ETH Added"
                    userDEthInfo.actualCollateralAdded
                , depositRedeemInfoItemEl
                    textFontSize
                    "Deposit Protocol Fee"
                    userDEthInfo.depositFee.protocolFee
                , depositRedeemInfoItemEl
                    textFontSize
                    "Deposit Automation Fee"
                    userDEthInfo.depositFee.automationFee
                , depositRedeemInfoItemEl
                    textFontSize
                    "dETH Received"
                    userDEthInfo.tokensIssued
                ]

            else
                [ depositRedeemInfoItemEl
                    textFontSize
                    "Total ETH Received"
                    userDEthInfo.collateralReturned
                , depositRedeemInfoItemEl
                    textFontSize
                    "Total ETH Redeemable"
                    userDEthInfo.totalCollateralRedeemed
                , depositRedeemInfoItemEl
                    textFontSize
                    "Withdrawal Protocol Fee"
                    userDEthInfo.redeemFee.protocolFee
                , depositRedeemInfoItemEl
                    textFontSize
                    "Withdrawal Automation Fee"
                    userDEthInfo.redeemFee.automationFee
                ]
    in
    elems
        |> column
            ([ width fill
             , spacing 5
             , padding 5
             ]
                ++ Theme.childContainerBackgroundAttributes
                ++ Theme.childContainerBorderAttributes
            )


depositRedeemInfoItemEl : Attribute Msg -> String -> TokenValue -> Element Msg
depositRedeemInfoItemEl textFontSize rowLabel rowValue =
    [ text
        (rowLabel ++ ":")
        |> el
            [ textFontSize ]
    , rowValue
        |> tokenValueToString
        |> text
        |> el
            [ textFontSize
            , alignRight
            ]
    ]
        |> row
            [ width fill
            ]


percentageButtonsEl : DisplayProfile -> (String -> Msg) -> TokenValue -> Element Msg
percentageButtonsEl dProfile buttonMsg userBalance =
    let
        buttonStyle =
            [ Font.color Theme.almostWhite
            , Font.size (responsiveVal dProfile 14 12)
            ]
    in
    [ buttonEl
        dProfile
        buttonStyle
        "25%"
        ((TokenValue.div (TokenValue.mul userBalance 25) 100
            |> tokenValueToString
            |> buttonMsg
         )
            |> Just
        )
    , buttonEl
        dProfile
        buttonStyle
        "50%"
        ((TokenValue.div (TokenValue.mul userBalance 50) 100
            |> tokenValueToString
            |> buttonMsg
         )
            |> Just
        )
    , buttonEl
        dProfile
        buttonStyle
        "75%"
        ((TokenValue.div (TokenValue.mul userBalance 75) 100
            |> tokenValueToString
            |> buttonMsg
         )
            |> Just
        )
    , buttonEl
        dProfile
        buttonStyle
        "100%"
        ((userBalance
            |> tokenValueToString
            |> buttonMsg
         )
            |> Just
        )
    ]
        |> row
            [ spacing 12
            , centerX
            ]


inputEl :
    DisplayProfile
    -> String
    -> TokenValue
    -> (String -> Msg)
    -> Element Msg
inputEl dProfile inputAmount userBalance msg =
    let
        amountElWidth =
            responsiveVal
                dProfile
                150
                100

        amountElHeight =
            responsiveVal
                dProfile
                30
                23

        inputValid =
            validateInput inputAmount userBalance

        inputStyles =
            [ px amountElWidth
                |> width
            , px amountElHeight
                |> height
            , Element.rgba 1 1 1 0.3
                |> Element.Background.color
            , padding 3
            , Element.Border.width 0
            , centerX
            , Font.size (responsiveVal dProfile 20 14)
            ]
                ++ (if inputValid /= Types.InputUndefined && inputValid /= Types.InputValid then
                        [ Element.Border.width 2
                        , Element.Border.color <| Theme.darkRed
                        ]

                    else
                        []
                   )
    in
    { onChange = msg
    , text = inputAmount
    , placeholder =
        text "Enter Amount"
            |> Element.Input.placeholder
                [ Font.color Theme.almostWhite
                ]
            |> Just
    , label = Element.Input.labelHidden "amount"
    }
        |> Element.Input.text
            inputStyles


buttonEl :
    DisplayProfile
    -> List (Attribute Msg)
    -> String
    -> Maybe Msg
    -> Element Msg
buttonEl dProfile attributes buttonLabel msg =
    { onPress = msg
    , label = text buttonLabel
    }
        |> Element.Input.button
            ([ padding 5
             , Element.Border.rounded 5
             , Element.Border.glow Theme.lightGray 1
             , centerX
             , Font.size (responsiveVal dProfile 18 12)
             ]
                ++ Theme.childContainerBackgroundAttributes
                ++ attributes
            )


validateInput : String -> TokenValue -> InputValidationResult
validateInput input max =
    case TokenValue.fromString input of
        Nothing ->
            Types.InputUndefined

        Just val ->
            if TokenValue.compare val TokenValue.zero == LT then
                Types.InputLessThan

            else if TokenValue.compare val max == GT then
                Types.InputGreaterThan

            else
                Types.InputValid


msgInsteadOfButton : DisplayProfile -> String -> Color -> Element Msg
msgInsteadOfButton dProfile textToDisplay color =
    [ textToDisplay
        |> text
    ]
        |> paragraph []
        |> el
            [ centerX
            , responsiveVal dProfile 18 12
                |> Font.size
            , Font.italic
            , Font.color color
            ]


tokenValueToString : TokenValue -> String
tokenValueToString tokenValue =
    let
        decimalWidth =
            5

        evm =
            tokenValue
                |> TokenValue.getEvmValue
                |> BigInt.toString

        evmLength =
            evm
                |> String.length
    in
    if evmLength <= decimalWidth then
        evm
            |> String.padLeft decimalWidth '0'
            |> (++) "0."

    else
        evm
            |> String.right decimalWidth
            |> (++)
                "."
            |> (++)
                (evm
                    |> String.left
                        (evmLength - decimalWidth)
                )
