module Theme exposing (..)

import Element exposing (Attribute, Color, Element, above, centerX, centerY, el, maximum, moveUp, padding, paddingXY, paragraph, rgb, rgba, shrink, text, width)
import Element.Background
import Element.Border
import Element.Font
import ElementHelpers as EH


type alias Theme msg =
    { headerBackground : Color
    , headerTextColor : Color
    , appBackground : Color
    , blockBackground : Color
    , txTrackerBackground : Color
    , mainTextColor : Color
    , linkTextColor : Color
    , linkTextColorAgainstBackground : Color
    , emphasizedTextColor : Color
    , postBodyTextColor : Color
    , messageInputPlaceholderTextColor : Color
    , loadingTextColor : Color
    , errorTextColor : Color
    , appStatusTextColor : Color
    , daiBurnedBackground : Color
    , daiBurnedTextIsWhite : Bool
    , daiTippedBackground : Color
    , daiTippedTextIsWhite : Bool
    , emphasizedActionButton : EH.DisplayProfile -> List (Attribute msg) -> List String -> EH.ButtonAction msg -> Element msg
    , secondaryActionButton : EH.DisplayProfile -> List (Attribute msg) -> List String -> EH.ButtonAction msg -> Element msg
    , disabledActionButton : EH.DisplayProfile -> List (Attribute msg) -> String -> Maybe String -> Element msg
    }


defaultTheme : Theme msg
defaultTheme =
    basicTheme


basicTheme : Theme msg
basicTheme =
    { headerBackground = darkBlue
    , headerTextColor = EH.white
    , appBackground = darkerBlue
    , blockBackground = lightBlue
    , txTrackerBackground = lightBlue
    , mainTextColor = EH.white
    , linkTextColor = blue
    , linkTextColorAgainstBackground = Element.rgb 0.4 0.6 1
    , emphasizedTextColor = EH.white
    , postBodyTextColor = EH.white
    , messageInputPlaceholderTextColor = almostWhite
    , loadingTextColor = almostWhite
    , errorTextColor = softRed
    , appStatusTextColor = almostWhite
    , daiBurnedBackground = lightRed
    , daiBurnedTextIsWhite = False
    , daiTippedBackground = lightGreen
    , daiTippedTextIsWhite = False
    , emphasizedActionButton = redButton
    , secondaryActionButton = blueButton
    , disabledActionButton = disabledButton
    }


softRed =
    Element.rgb255 255 0 110


darkRed =
    Element.rgb 0.7 0 0


darkGray =
    Element.rgb255 150 150 150


red =
    Element.rgb255 226 1 79


blue =
    Element.rgb 0 0 1


darkBlue =
    Element.rgb255 7 27 92


darkerBlue =
    Element.rgb255 7 20 60


lightGray =
    Element.rgb255 233 237 242


lightBlue =
    Element.rgb 0.8 0.8 1


almostWhite =
    Element.rgb 0.8 0.8 0.8


lightRed =
    Element.rgb 1 0.8 0.8


lightGreen =
    Element.rgb 0.8 1 0.8


veryDarkGray =
    Element.rgb 0.1 0.1 0.1


green =
    Element.rgb255 51 183 2


yellow =
    Element.rgb 1 1 0


darkYellow =
    Element.rgb 0.6 0.6 0


commonShadow : Attribute msg
commonShadow =
    Element.Border.shadow
        { offset = ( 0, 0 )
        , size = 0
        , blur = 10
        , color = darkGray
        }


blueButton : EH.DisplayProfile -> List (Attribute msg) -> List String -> EH.ButtonAction msg -> Element msg
blueButton dProfile attributes text action =
    EH.button dProfile
        attributes
        ( Element.rgba 0 0 1 1
        , Element.rgba 0 0 1 0.8
        , Element.rgba 0 0 1 0.6
        )
        EH.white
        text
        action


lightBlueButton : EH.DisplayProfile -> List (Attribute msg) -> List String -> EH.ButtonAction msg -> Element msg
lightBlueButton dProfile attributes text action =
    let
        color =
            Element.rgb255 25 169 214
    in
    EH.button dProfile
        attributes
        ( color
        , color |> EH.withAlpha 0.8
        , color |> EH.withAlpha 0.6
        )
        EH.white
        text
        action


inverseBlueButton : EH.DisplayProfile -> List (Attribute msg) -> List String -> EH.ButtonAction msg -> Element msg
inverseBlueButton dProfile attributes text action =
    EH.button dProfile
        attributes
        ( Element.rgba 0 0 1 0.05
        , Element.rgba 0 0 1 0.1
        , Element.rgba 0 0 1 0.2
        )
        blue
        text
        action


redButton : EH.DisplayProfile -> List (Attribute msg) -> List String -> EH.ButtonAction msg -> Element msg
redButton dProfile attributes text action =
    EH.button
        dProfile
        attributes
        ( Element.rgba 1 0 0 1
        , Element.rgba 1 0 0 0.8
        , Element.rgba 1 0 0 0.6
        )
        EH.white
        text
        action


disabledButton : EH.DisplayProfile -> List (Attribute msg) -> String -> Maybe String -> Element msg
disabledButton dProfile attributes textToDisplay maybeTipText =
    (text textToDisplay
        |> el
            [ centerY
            , centerX
            ]
    )
        |> el
            ([ Element.Border.rounded 4
             , EH.responsiveVal
                dProfile
                (paddingXY 25 17)
                (padding 10)
             , Element.Font.size
                (EH.responsiveVal
                    dProfile
                    18
                    16
                )
             , Element.Font.semiBold
             , Element.Background.color lightGray
             , Element.Font.center
             , EH.noSelectText
             , maybeErrorElement
                [ moveUp 5 ]
                maybeTipText
                |> above
             ]
                ++ attributes
            )


maybeErrorElement : List (Attribute msg) -> Maybe String -> Element msg
maybeErrorElement attributes maybeError =
    case maybeError of
        Nothing ->
            Element.none

        Just errorString ->
            ([ text errorString ]
                |> paragraph
                    []
            )
                |> el
                    ([ Element.Border.rounded 5
                     , Element.Border.color softRed
                     , Element.Border.width 1
                     , rgb 1 0.4 0.4
                        |> Element.Background.color
                     , padding 5
                     , centerX
                     , centerY
                     , width
                        (shrink
                            |> maximum 200
                        )
                     , Element.Font.size 14
                     ]
                        ++ attributes
                    )


mainContainerBorderAttributes : List (Attribute msg)
mainContainerBorderAttributes =
    [ Element.Border.rounded 10
    , Element.Border.glow EH.white 2
    ]


mainContainerBackgroundAttributes : List (Attribute msg)
mainContainerBackgroundAttributes =
    [ rgba 1 1 1 0.1
        |> Element.Background.color
    ]


childContainerBorderAttributes : List (Attribute msg)
childContainerBorderAttributes =
    [ Element.Border.rounded 5
    , Element.Border.glow lightGray 1
    ]


childContainerBackgroundAttributes : List (Attribute msg)
childContainerBackgroundAttributes =
    [ Element.rgba 1 1 1 0.3
        |> Element.Background.color
    ]
