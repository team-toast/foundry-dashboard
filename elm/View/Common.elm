module View.Common exposing (..)

import Common.Msg exposing (..)
import Common.Types exposing (..)
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import ElementMarkdown
import Eth.Types exposing (Address, Hex)
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..), responsiveVal)
import Helpers.Time as TimeHelpers
import Phace
import Routing exposing (Route)
import Theme exposing (defaultTheme)
import Time


shortenedHash : Hex -> String
shortenedHash hash =
    let
        hashStr =
            Eth.Utils.hexToString hash
    in
    if String.length hashStr <= 10 then
        hashStr

    else
        String.left 6 hashStr
            ++ "..."
            ++ String.right 4 hashStr


web3ConnectButton :
    EH.DisplayProfile
    -> List (Attribute msg)
    -> (MsgUp -> msg)
    -> Element msg
web3ConnectButton dProfile attrs msgMapper =
    defaultTheme.emphasizedActionButton
        dProfile
        attrs
        [ "Connect to Wallet" ]
        (msgMapper ConnectToWeb3)


phaceElement :
    Bool
    -> Address
    -> Bool
    -> DisplayProfile
    -> msg
    -> msg
    -> Element msg
phaceElement addressHangToRight fromAddress showAddress dProfile onClick noOpMsg =
    let
        phaceWidth =
            responsiveVal dProfile 100 30

        phaceHeight =
            responsiveVal dProfile 100 30

        addressOutputEl () =
            case dProfile of
                Desktop ->
                    -- delay processing because addressToChecksumString is expensive!
                    Element.el
                        [ Element.alignBottom
                        , if addressHangToRight then
                            Element.alignLeft

                          else
                            Element.alignRight
                        , Element.Background.color EH.white
                        , Element.Font.size 12
                        , EH.moveToFront
                        , Element.Border.width 2
                        , Element.Border.color EH.black
                        , EH.onClickNoPropagation noOpMsg
                        ]
                        (Element.text <| Eth.Utils.addressToChecksumString fromAddress)

                Mobile ->
                    -- delay processing because addressToChecksumString is expensive!
                    Element.el
                        [ Element.alignBottom
                        , if addressHangToRight then
                            Element.alignLeft

                          else
                            Element.alignRight
                        , Element.Background.color EH.white
                        , Element.Font.size 6
                        , EH.moveToFront
                        , Element.Border.width 2
                        , Element.Border.color EH.black
                        , EH.onClickNoPropagation noOpMsg
                        ]
                        (Element.text <| Eth.Utils.addressToChecksumString fromAddress)
    in
    Element.el
        (if showAddress then
            [ Element.inFront <| addressOutputEl ()
            , Element.alignTop
            ]

         else
            [ Element.alignTop ]
        )
    <|
        Element.el
            [ Element.Border.rounded 10
            , Element.clip
            , Element.pointer
            , EH.onClickNoPropagation onClick
            , Element.Border.width 2
            , Element.Border.color EH.black
            ]
        <|
            Element.html
                (Phace.fromEthAddress fromAddress)


loadingElement :
    List (Attribute msg)
    -> Maybe String
    -> Element msg
loadingElement attrs maybeString =
    Element.el
        ([ Element.Font.italic
         , Element.Font.color defaultTheme.loadingTextColor
         , Element.Font.size 20
         ]
            ++ attrs
        )
        (Element.text <| Maybe.withDefault "loading..." maybeString)


emphasizedText :
    String
    -> Element msg
emphasizedText =
    Element.el
        [ Element.Font.bold
        , Element.Font.color EH.white
        ]
        << Element.text


daiSymbol :
    Bool
    -> List (Attribute msg)
    -> Element msg
daiSymbol isWhite attributes =
    Element.image attributes
        { src =
            if isWhite then
                "img/dai-unit-char-white.svg"

            else
                "img/dai-unit-char-black.svg"
        , description = ""
        }


appStatusMessage :
    Element.Color
    -> String
    -> Element msg
appStatusMessage color errStr =
    Element.el [ Element.width Element.fill, Element.height Element.fill ] <|
        Element.paragraph
            [ Element.centerX
            , Element.centerY
            , Element.Font.center
            , Element.Font.italic
            , Element.Font.color color
            , Element.Font.size 36
            , Element.width (Element.fill |> Element.maximum 800)
            , Element.padding 40
            ]
            [ Element.text errStr ]


posixToString :
    Time.Posix
    -> String
posixToString t =
    let
        z =
            Time.utc
    in
    String.fromInt (Time.toYear z t)
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt <| TimeHelpers.monthToInt <| Time.toMonth z t)
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt (Time.toDay z t))
        ++ " "
        ++ String.padLeft 2 '0' (String.fromInt (Time.toHour z t))
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt (Time.toMinute z t))
        ++ " (UTC)"


subheaderAttributes :
    DisplayProfile
    -> List (Attribute msg)
subheaderAttributes dProfile =
    [ Element.paddingXY 0 <|
        responsiveVal
            dProfile
            20
            10
    , Element.Font.size <|
        responsiveVal
            dProfile
            50
            30
    , Element.Font.color defaultTheme.headerTextColor
    ]


commonFontSize :
    DisplayProfile
    -> Int
commonFontSize dProfile =
    case dProfile of
        Desktop ->
            24

        Mobile ->
            18


maxContentColWidth =
    1000


renderContentOrError :
    String
    -> Element msg
renderContentOrError content =
    let
        renderResult =
            ElementMarkdown.renderString
                [ Element.spacing 15
                , Element.Font.color defaultTheme.postBodyTextColor
                , Element.width Element.fill
                ]
                content
    in
    case renderResult of
        Ok rendered ->
            rendered

        Err errStr ->
            Element.el
                [ Element.Font.color defaultTheme.errorTextColor
                , Element.Font.italic
                ]
            <|
                Element.text <|
                    "Error parsing/rendering markdown: "
                        ++ errStr


daiAmountInput :
    DisplayProfile
    -> List (Attribute msg)
    -> String
    -> (String -> msg)
    -> Element msg
daiAmountInput dProfile attributes currentInput onChange =
    Element.Input.text
        [ Element.width <|
            Element.px <|
                responsiveVal
                    dProfile
                    100
                    60
        , Element.height <|
            Element.px <|
                responsiveVal
                    dProfile
                    40
                    35
        , Element.Font.size <|
            responsiveVal
                dProfile
                20
                14
        , Element.Background.color <| Element.rgba 1 1 1 0.4
        ]
        { onChange = onChange
        , text = currentInput
        , placeholder = Nothing
        , label = Element.Input.labelHidden "dai amount"
        }
