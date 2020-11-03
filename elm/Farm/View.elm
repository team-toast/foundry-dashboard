module Farm.View exposing (..)

import Common.Types exposing (..)
import Farm.Types exposing (..)
import Element exposing (Element)
import Helpers.Element as EH exposing (DisplayProfile)


view : DisplayProfile -> Maybe UserInfo -> Model -> Element Msg
view dProfile maybeUserInfo model =
    Element.none
