module View.DerivedEth exposing (view)

import BigInt
import Chain
import Element exposing (Attribute, Color, Element, above, alignRight, alignTop, alpha, centerX, column, el, fill, height, html, htmlAttribute, inFront, maximum, minimum, mouseOver, none, padding, paddingEach, paragraph, px, rgb, rgba, row, spacing, text, transparent, width)
import Element.Background
import Element.Border
import Element.Font as Font
import Element.Input
import ElementHelpers as EH exposing (DisplayProfile(..), responsiveVal)
import Html exposing (b, sup)
import Html.Attributes
import Maybe.Extra
import Misc exposing (derivedEthUserInfo, userInfo)
import Theme exposing (disabledButton, green, red, redButton)
import TokenValue exposing (TokenValue)
import Types exposing (Chain(..), DEthDepositInfo, DEthUserInfo, DEthWithdrawInfo, InputValidationError, JurisdictionCheckStatus, Model, Msg, UserDerivedEthInfo, UserInfo, Wallet(..))
import View.Common exposing (..)
import Wallet


type DepositOrRedeemInfo
    = Deposit (Maybe DEthDepositInfo)
    | Withdraw (Maybe DEthWithdrawInfo)


type TokenName
    = ETh String
    | DEth String


view : Model -> Element Msg
view model =
    let
        dProfile =
            model.dProfile

        walletState =
            model.wallet

        chain =
            model.wallet
                |> Wallet.getChainDefaultEth
    in
    [ titleEl dProfile
    , case chain of
        Eth ->
            mainEl
                dProfile
                walletState
                model.depositAmountInput
                model.withdrawalAmountInput
                model.dEthUserInfo
                model.dEthDepositInfo
                model.dEthWithdrawInfo

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
    paragraph
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
        [ html <|
            b []
                [ Html.text "\"(Eth is money)"
                , sup [] [ Html.text "2" ]
                , Html.text "=dEth\""
                ]
        ]


mainEl : DisplayProfile -> Wallet -> String -> String -> Maybe DEthUserInfo -> Maybe DEthDepositInfo -> Maybe DEthWithdrawInfo -> Element Msg
mainEl dProfile walletState depositAmount withdrawalAmount maybeUserDerivedEthInfo maybeDEthDepositInfo maybeDEthWithdrawInfo =
    let
        ethSymbol =
            ETh "ETH"

        dEThSymbol =
            DEth "dEth"

        dEthDepositInfoType =
            Deposit maybeDEthDepositInfo

        dEthWithdrawInfoType =
            Withdraw maybeDEthWithdrawInfo
    in
    [ investOrWithdrawEl
        dProfile
        walletState
        "ETH -> dEth"
        "Deposit"
        depositAmount
        dEthDepositInfoType
        maybeUserDerivedEthInfo
        ethSymbol
        Types.DepositAmountChanged
    , investOrWithdrawEl
        dProfile
        walletState
        "dEth -> ETH"
        "Redeem"
        withdrawalAmount
        dEthWithdrawInfoType
        maybeUserDerivedEthInfo
        dEThSymbol
        Types.WithdrawalAmountChanged
    ]
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
    -> Wallet
    -> String
    -> String
    -> String
    -> DepositOrRedeemInfo
    -> Maybe DEthUserInfo
    -> TokenName
    -> (String -> Msg)
    -> Element Msg
investOrWithdrawEl dProfile walletState heading buttonText inputAmount depositOrRedeemInfo maybeDethUserInfo tokenName msg =
    let
        textFontSize =
            Font.size (responsiveVal dProfile 22 16)

        headingFontSize =
            Font.size (responsiveVal dProfile 28 18)

        ( amountChangedMsg, clickedMsg ) =
            case depositOrRedeemInfo of
                Deposit _ ->
                    ( Types.DepositAmountChanged, Types.DepositClicked )

                Withdraw _ ->
                    ( Types.WithdrawalAmountChanged, Types.WithdrawClicked )

        msgAmountResult =
            maybeUserBalance
                |> Maybe.andThen (validateInput inputAmount)

        maybeUserBalance =
            case depositOrRedeemInfo of
                Deposit _ ->
                    maybeDethUserInfo |> Maybe.map .ethBalance

                Withdraw _ ->
                    maybeDethUserInfo |> Maybe.map .dEthBalance

        blockHeightMin =
            responsiveVal dProfile 280 220

        tokenStringName =
            case tokenName of
                ETh tokenString ->
                    tokenString

                DEth tokenString ->
                    tokenString
    in
    [ text heading
        |> el
            [ headingFontSize
            , Font.semiBold
            , centerX
            ]
    , case maybeUserBalance of
        Nothing ->
            Element.none

        Just userBalance ->
            text
                (tokenStringName
                    ++ " balance: "
                    ++ (userBalance
                            |> TokenValue.toFloatString (Just 4)
                       )
                )
                |> el
                    [ textFontSize
                    , centerX
                    ]
    , [ case maybeUserBalance of
            Nothing ->
                Element.none

            Just userBalance ->
                percentageButtonsEl
                    dProfile
                    amountChangedMsg
                    userBalance
      , [ inputEl
            dProfile
            inputAmount
            maybeUserBalance
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
            walletState
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
        Just (Err validationError) ->
            --inputErrorEl dProfile (validationErrorToString validationError)--
            depositRedeemInfoEl
                dProfile
                inputAmount
                depositOrRedeemInfo
                |> el
                    [ width fill
                    , paddingEach
                        { top = 0
                        , left = responsiveVal dProfile 20 10
                        , right = responsiveVal dProfile 20 10
                        , bottom = 0
                        }
                    ]

        _ ->
            depositRedeemInfoEl
                dProfile
                inputAmount
                depositOrRedeemInfo
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


depositRedeemInfoEl : DisplayProfile -> String -> DepositOrRedeemInfo -> Element Msg
depositRedeemInfoEl dProfile amountEntered depositOrRedeemInfo =
    let
        enteredAmount =
            Maybe.withDefault TokenValue.zero <| TokenValue.fromString amountEntered

        textFontSize =
            responsiveVal dProfile 16 10
                |> Font.size

        zeroTokenValue =
            TokenValue.zero

        elems =
            case depositOrRedeemInfo of
                Deposit maybeDEthDepositInfo ->
                    [ depositRedeemInfoItemEl
                        textFontSize
                        "ETH Amount Entered"
                        (Just enteredAmount)
                    , depositRedeemInfoItemEl
                        textFontSize
                        "Protocol Fee"
                        (maybeDEthDepositInfo |> Maybe.map (.depositFee >> .protocolFee))
                    , depositRedeemInfoItemEl
                        textFontSize
                        "Automation Fee"
                        (maybeDEthDepositInfo |> Maybe.map (.depositFee >> .automationFee))
                    , depositRedeemInfoItemEl
                        textFontSize
                        "Actual ETH Added"
                        (maybeDEthDepositInfo |> Maybe.map .actualCollateralAdded)
                    , depositRedeemInfoItemEl
                        textFontSize
                        "dEth Issued"
                        (maybeDEthDepositInfo |> Maybe.map .tokensIssued)
                    ]

                Withdraw maybeDEthWithdrawInfo ->
                    [ depositRedeemInfoItemEl
                        textFontSize
                        "dEth Amount Entered"
                        (Just enteredAmount)
                    , depositRedeemInfoItemEl
                        textFontSize
                        "Automation Fee"
                        (maybeDEthWithdrawInfo |> Maybe.map (.redeemFee >> .automationFee))
                    , depositRedeemInfoItemEl
                        textFontSize
                        "Collateral Redeemed"
                        (maybeDEthWithdrawInfo |> Maybe.map .totalCollateralRedeemed)
                    , depositRedeemInfoItemEl
                        textFontSize
                        "Protocol Fee"
                        (maybeDEthWithdrawInfo |> Maybe.map (.redeemFee >> .protocolFee))
                    , depositRedeemInfoItemEl
                        textFontSize
                        "Collateral Returned"
                        (maybeDEthWithdrawInfo |> Maybe.map .collateralReturned)
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


depositRedeemInfoItemEl : Attribute Msg -> String -> Maybe TokenValue -> Element Msg
depositRedeemInfoItemEl textFontSize rowLabel maybeRowValue =
    [ text
        (rowLabel ++ ":")
        |> el
            [ textFontSize ]
    , case maybeRowValue of
        Just rowValue ->
            rowValue
                |> tokenValueToFixedPrecisionFloatString 4
                |> text
                |> el
                    [ textFontSize
                    , alignRight
                    ]

        Nothing ->
            Element.none
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
    -> Maybe TokenValue
    -> (String -> Msg)
    -> Element Msg
inputEl dProfile inputAmount maybeUserBalance msg =
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
            maybeUserBalance
                |> Maybe.andThen (validateInput inputAmount)

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


buttonStateEl :
    DisplayProfile
    -> String
    -> Maybe Msg
    -> Maybe (Result InputValidationError TokenValue)
    -> Wallet
    -> Element Msg
buttonStateEl dProfile buttonLabel msg amountResult walletState =
    let
        buttonErrorStyle =
            [ htmlAttribute <| Html.Attributes.style "cursor" "not-allowed"
            , Element.Background.color red
            , alpha 0.5
            ]

        buttonLoadingStyle =
            [ htmlAttribute <| Html.Attributes.style "cursor" "not-allowed"
            , Element.Background.color green
            , alpha 0.5
            ]

        buttonNormalStyle =
            Theme.childContainerBackgroundAttributes
    in
    case amountResult of
        Nothing ->
            case walletState of
                NoneDetected ->
                    el [ tooltip above (buttonTooltip "Connect Wallet!") ]
                        (buttonEl
                            dProfile
                            buttonErrorStyle
                            buttonLabel
                            msg
                        )

                NetworkReady ->
                    el [ tooltip above (buttonTooltip "Connect Wallet!") ]
                        (buttonEl
                            dProfile
                            buttonErrorStyle
                            buttonLabel
                            msg
                        )

                Connecting ->
                    el [ tooltip above (buttonTooltip "Connecting Wallet!") ]
                        (buttonEl
                            dProfile
                            buttonLoadingStyle
                            buttonLabel
                            msg
                        )

                Active _ ->
                    buttonEl
                        dProfile
                        buttonNormalStyle
                        buttonLabel
                        msg

        Just (Err validationError) ->
            case validationError of
                Types.InputGreaterThan ->
                    el [ tooltip above (buttonTooltip "You don't have that much") ]
                        (buttonEl
                            dProfile
                            buttonErrorStyle
                            buttonLabel
                            msg
                        )

                Types.InputLessThan ->
                    el [ tooltip above (buttonTooltip "Need a positive number") ]
                        (buttonEl
                            dProfile
                            buttonErrorStyle
                            buttonLabel
                            msg
                        )

                Types.InputInvalid ->
                    el [ tooltip above (buttonTooltip "Can't interpret that number!") ]
                        (buttonEl
                            dProfile
                            buttonErrorStyle
                            buttonLabel
                            msg
                        )

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
