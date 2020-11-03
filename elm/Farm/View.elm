module Farm.View exposing (..)

import Common.Types exposing (..)
import Farm.Types exposing (..)
import Element exposing (Element)
import Helpers.Element as EH exposing (DisplayProfile)
import Common.View


view : DisplayProfile -> Maybe UserInfo -> Model -> Element Msg
view dProfile maybeUserInfo model =
    case maybeUserInfo of
        Nothing ->
            Common.View.web3ConnectButton
                dProfile
                [ Element.centerX
                , Element.centerY
                ]
                MsgUp
        Just userInfo ->
            Element.text "Next up, fetch all data necessary. But what is that? Gotta write a fetch script in Solidity."