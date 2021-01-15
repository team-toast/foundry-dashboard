module DerivedEth.View exposing (view)

import Common.Types exposing (..)
import Common.View exposing (..)
import Config
import DerivedEth.Types exposing (..)
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Attribute, Element, centerX, column, el, padding, paragraph, row, spacing, text)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Eth.Types exposing (Address)
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..), responsiveVal)
import Helpers.Tuple as TupleHelpers
import Routing exposing (Route)
import Theme exposing (darkTheme, defaultTheme)
import Time
import TokenValue exposing (TokenValue)
import Wallet exposing (Wallet)


view :
    EH.DisplayProfile
    -> Maybe UserInfo
    -> Model
    -> Element Msg
view dProfile maybeUserInfo model =
    [ titleEl dProfile
    ]
        |> responsiveVal
            dProfile
            row
            column
            ([ padding 20
             , spacing 25
             , centerX
             , Element.Font.color EH.white
             ]
                ++ Theme.whiteGlowOuterRounded
            )
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
            ]


titleEl :
    DisplayProfile
    -> Element Msg
titleEl dProfile =
    text "dETH - Prepare to have your face ripped off by the Dragon..."
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
            , centerX
            ]
