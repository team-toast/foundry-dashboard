module DerivedEth.View exposing (view)

import Common.Types exposing (..)
import Common.View exposing (..)
import Config
import DerivedEth.Types exposing (..)
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Attribute, Element, centerX, column, padding, row, spacing, text)
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
    [ text "dEth will rip your face off"
    ]
        |> responsiveVal
            dProfile
            row
            column
            [ padding 20
            , spacing 25
            , centerX
            ]
