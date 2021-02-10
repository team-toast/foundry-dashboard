module View.Home exposing (view)

import Element exposing (Element)
import Types exposing (Model, Msg)
import View.Common exposing (..)


view : Model -> Element Msg
view model =
    Element.text "home :3"
