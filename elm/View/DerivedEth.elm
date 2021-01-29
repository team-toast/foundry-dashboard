module View.DerivedEth exposing (view)

import Element exposing (Attribute, Color, Element, alignRight, alignTop, centerX, column, el, fill, height, minimum, padding, paddingEach, paragraph, px, row, spacing, text, width)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import ElementHelpers as EH exposing (DisplayProfile(..), responsiveVal)
import Misc exposing (derivedEthInfoInit, userInfo)
import Theme exposing (disabledButton, green, red, redButton)
import TokenValue exposing (TokenValue)
import Types exposing (InputValidationResult, JurisdictionCheckStatus, Model, Msg, UserDerivedEthInfo, UserInfo)
import View.Common exposing (..)


view : Model -> Element Msg
view model =
    let
        dProfile =
            model.dProfile
    in
    [ titleEl dProfile
    , mainEl
        dProfile
        model.depositAmount
        model.withDrawalAmount
        (userInfo model.wallet)
        model.userDerivedEthInfo
    ]
        |> column
            [ padding 20
            , spacing (responsiveVal dProfile 25 10)
            , Element.Font.color EH.white
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
                |> Element.Font.size
            , Element.Font.color EH.white
            , responsiveVal
                dProfile
                Element.Font.bold
                Element.Font.semiBold
            , centerX
            , padding 20
            ]


mainEl : DisplayProfile -> String -> String -> Maybe UserInfo -> Maybe UserDerivedEthInfo -> Element Msg
mainEl dProfile depositAmount withdrawalAmount maybeUserInfo maybeUserDerivedEthInfo =
    (case maybeUserDerivedEthInfo of
        Nothing ->
            [ text "Loading user info..." ]

        Just userDerivedEthInfo ->
            [ investOrWithdrawEl
                dProfile
                "Squander your ETH for worthless beans"
                "Deposit"
                depositAmount
                "ETH"
                userDerivedEthInfo.ethBalance
                Types.DepositAmountChanged
                maybeUserDerivedEthInfo
            , investOrWithdrawEl
                dProfile
                "Redeem worthless beans for ETH"
                "Redeem"
                withdrawalAmount
                "dETH"
                userDerivedEthInfo.dEthBalance
                Types.WithdrawalAmountChanged
                maybeUserDerivedEthInfo
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
    -> TokenValue
    -> (String -> Msg)
    -> Maybe UserDerivedEthInfo
    -> Element Msg
investOrWithdrawEl dProfile heading buttonText inputAmount tokenName userBalance msg maybeUserDerivedEthInfo =
    let
        inputValid =
            validateInput inputAmount userBalance

        textFontSize =
            Element.Font.size (responsiveVal dProfile 22 16)

        headingFontSize =
            Element.Font.size (responsiveVal dProfile 28 18)

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

        userDEthInfo =
            Maybe.withDefault derivedEthInfoInit maybeUserDerivedEthInfo

        blockHeightMin =
            responsiveVal dProfile 220 220
    in
    [ text heading
        |> el
            [ headingFontSize
            , Element.Font.semiBold
            , centerX
            ]
    , text
        (tokenName
            ++ " balance: "
            ++ (userBalance
                    |> TokenValue.toFloatWithWarning
                    |> String.fromFloat
               )
        )
        |> el
            [ textFontSize
            , centerX
            ]
    , responsiveVal dProfile
        Element.none
        (percentageButtonsEl
            dProfile
            amountChangedMsg
            userBalance
        )
    , [ responsiveVal dProfile
            (percentageButtonsEl
                dProfile
                amountChangedMsg
                userBalance
            )
            Element.none
      , inputEl
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
                            , left = 20
                            , right = 20
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
                   , width fill
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
            responsiveVal dProfile 16 12
                |> Element.Font.size

        ( text1, text2, text3 ) =
            if tokenName == "ETH" then
                ( "Actual ETH Added:", "Deposit Fee:", "dETH received:" )

            else
                ( "Total ETH Redeemable:", "Withdrawal Fee:", "Total ETH received:" )

        ( val1, val2, val3 ) =
            if tokenName == "ETH" then
                ( userDEthInfo.actualCollateralAdded
                    |> TokenValue.toFloatWithWarning
                    |> String.fromFloat
                , userDEthInfo.depositFee
                    |> TokenValue.toFloatWithWarning
                    |> String.fromFloat
                , userDEthInfo.tokensIssued
                    |> TokenValue.toFloatWithWarning
                    |> String.fromFloat
                )

            else
                ( userDEthInfo.totalCollateralRedeemed
                    |> TokenValue.toFloatWithWarning
                    |> String.fromFloat
                , userDEthInfo.redeemFee
                    |> TokenValue.toFloatWithWarning
                    |> String.fromFloat
                , userDEthInfo.collateralReturned
                    |> TokenValue.toFloatWithWarning
                    |> String.fromFloat
                )
    in
    [ depositRedeemInfoItemEl
        textFontSize
        text1
        val1
    , depositRedeemInfoItemEl
        textFontSize
        text2
        val2
    , depositRedeemInfoItemEl
        textFontSize
        text3
        val3
    ]
        |> column
            ([ width fill
             , spacing 5
             , padding 5
             ]
                ++ Theme.childContainerBackgroundAttributes
                ++ Theme.childContainerBorderAttributes
            )


depositRedeemInfoItemEl : Attribute Msg -> String -> String -> Element Msg
depositRedeemInfoItemEl textFontSize rowLabel rowValue =
    [ text
        rowLabel
        |> el
            [ textFontSize ]
    , text
        rowValue
        |> el
            [ textFontSize
            , alignRight
            ]
    ]
        |> row
            [ width fill
            ]


percentageButtonsEl : DisplayProfile -> (String -> Msg) -> TokenValue -> Element Msg
percentageButtonsEl dProfile msg2 userBalance =
    let
        buttonStyle =
            [ Element.Font.color Theme.almostWhite
            , Element.Font.size (responsiveVal dProfile 14 12)
            ]
    in
    [ buttonEl
        dProfile
        buttonStyle
        "25%"
        ((TokenValue.toFloatWithWarning userBalance
            * 0.25
            |> String.fromFloat
            |> msg2
         )
            |> Just
        )
    , buttonEl
        dProfile
        buttonStyle
        "50%"
        ((TokenValue.toFloatWithWarning userBalance
            * 0.5
            |> String.fromFloat
            |> msg2
         )
            |> Just
        )
    , buttonEl
        dProfile
        buttonStyle
        "75%"
        ((TokenValue.toFloatWithWarning userBalance
            * 0.75
            |> String.fromFloat
            |> msg2
         )
            |> Just
        )
    , buttonEl
        dProfile
        buttonStyle
        "100%"
        ((userBalance
            |> TokenValue.toFloatWithWarning
            |> String.fromFloat
            |> msg2
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
            , Element.Font.size (responsiveVal dProfile 20 14)
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
                [ Element.Font.color Theme.almostWhite
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
             , Element.Font.size (responsiveVal dProfile 18 12)
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
    [ text textToDisplay ]
        |> paragraph []
        |> el
            [ centerX
            , Element.Font.size <|
                responsiveVal
                    dProfile
                    18
                    12
            , Element.Font.italic
            , Element.Font.color color
            ]
