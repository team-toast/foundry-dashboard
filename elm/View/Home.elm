module View.Home exposing (view)

import Common.Types exposing (..)
import Config
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..), responsiveVal)
import Helpers.Tuple as TupleHelpers
import Home.Types exposing (..)
import Routing exposing (Route)
import Theme exposing (darkTheme, defaultTheme)
import TokenValue exposing (TokenValue)
import View.Common exposing (..)
import Wallet exposing (Wallet)


view : EH.DisplayProfile -> Maybe UserInfo -> Model -> Element Msg
view dProfile maybeUserInfo model =
    Element.text "home :3"
