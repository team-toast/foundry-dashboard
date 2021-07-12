module View.DerivedEth exposing (view)

import BigInt
import Chain
import Element exposing (Attribute, Color, Element, alignRight, alignTop, alpha,centerX, column, el, fill, height, htmlAttribute, maximum, minimum, padding, paddingEach, paragraph, px, row,rgb,rgba, spacing, text, width)
import Element.Background
import Element.Border
import Element.Font as Font
import Element.Input
import ElementHelpers as EH exposing (DisplayProfile(..), responsiveVal)
import Maybe.Extra
import Misc exposing (userInfo)
import Theme exposing (disabledButton, green, red, redButton)
import TokenValue exposing (TokenValue)
import Types exposing (Chain(..), InputValidationError, JurisdictionCheckStatus, Model, Msg, UserDerivedEthInfo, UserInfo)
import View.Common exposing (..)
import Wallet
import Html.Attributes
import Element exposing (mouseOver)
import Element exposing (inFront)
import Element exposing (transparent)
import Element exposing (none)
import Element exposing (above)


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
    [ text "\"Ride the Dragon.\" - dEth" ]
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
                        "ETH -> dEth"
                        "Deposit"
                        depositAmount
                        "ETH"
                        Types.DepositAmountChanged
                        userDerivedEthInfo
                    , investOrWithdrawEl
                        dProfile
                        "dEth -> ETH"
                        "Redeem"
                        withdrawalAmount
                        "dEth"
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


validationErrorToString : InputValidationError -> String
validationErrorToString validationError =
    case validationError of
        Types.InputGreaterThan ->
            "You don't have that much!"

        Types.InputLessThan ->
            "Need a positive number!"

        Types.InputInvalid ->
            "Can't interpret that number!"


inputErrorEl : DisplayProfile -> String -> Element Msg
inputErrorEl dProfile errStr =
    Element.rgba 1 0 0 0.8
        |> msgInsteadOfButton
            dProfile
            errStr


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

        msgAmountResult =
            validateInput inputAmount userBalance

        userBalance =
            if tokenName == "ETH" then
                userDEthInfo.ethBalance

            else
                userDEthInfo.dEthBalance

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
                    |> TokenValue.toFloatString (Just 4)
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
        , buttonStateEl
            dProfile
            buttonText
            (msgAmountResult
                |> Maybe.map Result.toMaybe
                |> Maybe.Extra.join
                |> Maybe.map clickedMsg
            )
            msgAmountResult

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
    , case msgAmountResult of
        Nothing ->
            Element.none

        Just (Err validationError) ->
            --inputErrorEl dProfile (validationErrorToString validationError)--
            depositRedeemInfoEl
                dProfile
                tokenName
                inputAmount
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

        Just (Ok _) ->
            depositRedeemInfoEl
                dProfile
                tokenName
                inputAmount
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
                   , Font.family [ Font.monospace ]
                   ]
            )


depositRedeemInfoEl : DisplayProfile -> String -> String -> UserDerivedEthInfo -> Element Msg
depositRedeemInfoEl dProfile tokenName amountEntered userDEthInfo =
    let
        enteredAmount =
            amountToTokenValue <| TokenValue.fromString amountEntered

        textFontSize =
            responsiveVal dProfile 16 10
                |> Font.size

        elems =
            if tokenName == "ETH" then
                [ depositRedeemInfoItemEl
                    textFontSize
                    "ETH Amount Entered"
                    enteredAmount
                , depositRedeemInfoItemEl
                    textFontSize
                    "Protocol Fee"
                    userDEthInfo.depositFee.protocolFee
                , depositRedeemInfoItemEl
                    textFontSize
                    "Automation Fee"
                    userDEthInfo.depositFee.automationFee
                , depositRedeemInfoItemEl
                    textFontSize
                    "Actual ETH Added"
                    userDEthInfo.actualCollateralAdded
                , depositRedeemInfoItemEl
                    textFontSize
                    "dEth Issued"
                    userDEthInfo.tokensIssued
                ]

            else
                [ depositRedeemInfoItemEl
                    textFontSize
                    "dEth Amount Entered"
                    enteredAmount
                , depositRedeemInfoItemEl
                    textFontSize
                    "Automation Fee"
                    userDEthInfo.redeemFee.automationFee
                , depositRedeemInfoItemEl
                    textFontSize
                    "Collateral Redeemed"
                    userDEthInfo.totalCollateralRedeemed
                , depositRedeemInfoItemEl
                    textFontSize
                    "Protocol Fee"
                    userDEthInfo.redeemFee.protocolFee
                , depositRedeemInfoItemEl
                    textFontSize
                    "Collateral Returned"
                    userDEthInfo.collateralReturned
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
        |> tokenValueToFixedPrecisionFloatString(4)
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
            |> TokenValue.toFloatString (Just 4)
            |> buttonMsg
         )
            |> Just
        )
    , buttonEl
        dProfile
        buttonStyle
        "50%"
        ((TokenValue.div (TokenValue.mul userBalance 50) 100
            |> TokenValue.toFloatString (Just 4)
            |> buttonMsg
         )
            |> Just
        )
    , buttonEl
        dProfile
        buttonStyle
        "75%"
        ((TokenValue.div (TokenValue.mul userBalance 75) 100
            |> TokenValue.toFloatString (Just 4)
            |> buttonMsg
         )
            |> Just
        )
    , buttonEl
        dProfile
        buttonStyle
        "100%"
        ((userBalance
            |> TokenValue.toFloatString (Just 4)
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

        inputValidateResult =
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
                ++ (case inputValidateResult of
                        Just (Err _) ->
                            [ Element.Border.width 2
                            , Element.Border.color <| Theme.darkRed
                            ]

                        _ ->
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
                ++ attributes
            )
buttonStateEl:
    DisplayProfile
    -> String
    -> Maybe Msg
    -> Maybe (Result InputValidationError TokenValue)
    -> Element Msg
buttonStateEl dProfile  buttonLabel msg amountResult   =
    let
        buttonErrorStyle =
            [
             htmlAttribute <| Html.Attributes.style "cursor" "not-allowed"
            , Element.Background.color red
            , alpha 0.5
            ]
        buttonNormalStyle =
              Theme.childContainerBackgroundAttributes

    in

    case amountResult of

        Nothing ->
            buttonEl
            dProfile
            buttonNormalStyle
            buttonLabel
            msg
        Just (Err validationError) ->
            case validationError of
                Types.InputGreaterThan ->
                    el [ tooltip above (buttonTooltip "You don't have that much")]
                       (buttonEl
                       dProfile
                       buttonErrorStyle
                       buttonLabel
                       msg )

                Types.InputLessThan ->
                    el [ tooltip above (buttonTooltip "Need a positive number")]
                    (buttonEl
                    dProfile
                    buttonErrorStyle
                    buttonLabel
                    msg)

                Types.InputInvalid ->
                    el [ tooltip above ( buttonTooltip "Can't interpret that number!")]
                    (buttonEl
                    dProfile
                    buttonErrorStyle
                    buttonLabel
                    msg)
        Just (Ok _) ->
            buttonEl
            dProfile
            buttonNormalStyle
            buttonLabel
            msg




validateInput : String -> TokenValue -> Maybe (Result InputValidationError TokenValue)
validateInput input max =
    if String.trim input == "" then
        Nothing

    else
        Just <|
            case TokenValue.fromString input of
                Nothing ->
                    Err Types.InputInvalid

                Just val ->
                    if TokenValue.compare val TokenValue.zero == LT then
                        Err Types.InputLessThan

                    else if TokenValue.compare val max == GT then
                        Err Types.InputGreaterThan

                    else
                        Ok val


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


amountToTokenValue : Maybe TokenValue -> TokenValue
amountToTokenValue maybeAmount =
    case maybeAmount of
        Just amount ->
            amount

        Nothing ->
            TokenValue.zero

buttonTooltip : String -> Element msg
buttonTooltip str =
    el
        [ Element.Background.color (rgb 0 0 0)
        , Font.color (rgb 1 1 1)
        , padding 4
        , Element.Border.rounded 5
        , Font.size 14
        , Element.Border.shadow
            { offset = ( 0, 3 ), blur = 6, size = 0, color = rgba 0 0 0 0.32 }
        ]
        (text str)

tooltip : (Element msg -> Attribute msg) -> Element Never -> Attribute msg
tooltip usher tooltip_ =
    inFront <|
        el
            [ width fill
            , height fill
            , transparent True
            , mouseOver [ transparent False ]
            , (usher << Element.map never) <|
                el [ htmlAttribute (Html.Attributes.style "pointerEvents" "none") ]
                    tooltip_
            ]
            none

tokenValueToFixedPrecisionFloatString : Int -> TokenValue -> String
tokenValueToFixedPrecisionFloatString requiredDecimals tokens =
    let
        possiblyTooShortString =
            TokenValue.toFloatString (Just requiredDecimals) tokens

        maybePointIndex =
            String.indexes "." possiblyTooShortString
                |> List.head
    in
    case maybePointIndex of
        Just pointIndex ->
            let
                existingDecimals =
                    String.length possiblyTooShortString - (pointIndex + 1)

                extraNeededDecimals =
                    requiredDecimals - existingDecimals
            in
            possiblyTooShortString ++ String.repeat extraNeededDecimals "0"

        Nothing ->
            possiblyTooShortString ++ "." ++ String.repeat requiredDecimals "0"




