module DerivedEth.View exposing (view)

import Common.Types exposing (..)
import Common.View exposing (..)
import DerivedEth.Types exposing (..)
import Element exposing (Attribute, Element, centerX, column, el, fill, height, padding, paragraph, px, row, spacing, text, width)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Helpers.Element as EH exposing (DisplayProfile(..), responsiveVal)
import Theme exposing (darkTheme, defaultTheme)
import TokenValue exposing (TokenValue)
import Wallet exposing (Wallet)


view :
    DisplayProfile
    -> Maybe UserInfo
    -> Model
    -> Element Msg
view dProfile maybeUserInfo model =
    [ titleEl dProfile
    , mainEl
        dProfile
        model.depositAmount
        model.withDrawalAmount
        model.jurisdictionCheckStatus
        (Wallet.userInfo model.wallet)
        model.userDerivedEthInfo
    ]
        |> column
            [ padding 20
            , spacing 25
            , Element.Font.color EH.white
            , width fill
            ]
        |> el
            [ Element.paddingEach
                { top =
                    responsiveVal
                        dProfile
                        60
                        15
                , bottom = 0
                , left = 0
                , right = 0
                }
            , centerX
            ]


titleEl :
    DisplayProfile
    -> Element Msg
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
            ]


mainEl :
    DisplayProfile
    -> String
    -> String
    -> JurisdictionCheckStatus
    -> Maybe UserInfo
    -> Maybe UserDerivedEthInfo
    -> Element Msg
mainEl dProfile depositAmount withdrawalAmount jurisdictionCheckStatus maybeUserInfo maybeUserDerivedEthInfo =
    (case maybeUserInfo of
        Nothing ->
            [ web3ConnectButton dProfile
                [ Element.centerY
                , Element.centerX
                ]
                MsgUp
            ]

        Just userInfo ->
            case maybeUserDerivedEthInfo of
                Nothing ->
                    [ text "Loading user info..." ]

                Just userDerivedEthInfo ->
                    [ case jurisdictionCheckStatus of
                        Error err ->
                            text err

                        WaitingForClick ->
                            text "tick tock"

                        Checking ->
                            text "Verifying location"

                        Checked jurisdiction ->
                            case jurisdiction of
                                ForbiddenJurisdictions ->
                                    text "fuckoff SEC"

                                JurisdictionsWeArentIntimidatedIntoExcluding ->
                                    availableEthEl
                                        dProfile
                                        depositAmount
                                        userDerivedEthInfo.ethBalance
                    , availableDerivedEthEl
                        dProfile
                        withdrawalAmount
                        userDerivedEthInfo.dEthBalance
                    ]
    )
        |> column
            (Theme.whiteGlowOuterRounded
                ++ [ spacing 20
                   , padding 20
                   , width fill
                   ]
            )


availableEthEl :
    DisplayProfile
    -> String
    -> TokenValue
    -> Element Msg
availableEthEl dProfile amountToDeposit availableEth =
    [ text "ETH available to invest"
    , inputEl
        dProfile
        amountToDeposit
        availableEth
        DepositAmountChanged
    , buttonEl
        dProfile
        "Deposit"
        (Just DepositClicked)
    ]
        |> responsiveVal
            dProfile
            row
            column
            (Theme.whiteGlowInnerRounded ++ [])


availableDerivedEthEl :
    DisplayProfile
    -> String
    -> TokenValue
    -> Element Msg
availableDerivedEthEl dProfile amountToWithdraw availableDerivedEth =
    [ text "dETH available to redeem"
    , inputEl
        dProfile
        amountToWithdraw
        availableDerivedEth
        WithdrawalAmountChanged
    , buttonEl
        dProfile
        "Withdraw"
        (Just WithdrawClicked)
    ]
        |> responsiveVal
            dProfile
            row
            column
            (Theme.whiteGlowInnerRounded ++ [])


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
                75

        inputStyles =
            [ width <|
                px amountElWidth
            , Element.Background.color <| Element.rgba 1 1 1 0.3
            , padding 0
            , Element.Border.width 0
            ]
                ++ (if validateInput inputAmount userBalance == Nothing then
                        [ Element.Border.width 2
                        , Element.Border.color <| Theme.darkRed
                        ]

                    else
                        []
                   )
    in
    Element.Input.text
        inputStyles
        { onChange = msg
        , text = inputAmount
        , placeholder = Nothing
        , label = Element.Input.labelHidden "amount"
        }


buttonEl :
    DisplayProfile
    -> String
    -> Maybe Msg
    -> Element Msg
buttonEl dProfile buttonLabel msg =
    Element.Input.button
        []
        { onPress = msg
        , label = text buttonLabel
        }


validateInput :
    String
    -> TokenValue
    -> Maybe TokenValue
validateInput input max =
    TokenValue.fromString input
        |> Maybe.andThen
            (\val ->
                if TokenValue.compare val TokenValue.zero == LT then
                    Nothing

                else if TokenValue.compare val max == GT then
                    Nothing

                else
                    Just val
            )
