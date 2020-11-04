module Farm.View exposing (..)

import Common.Types exposing (..)
import Common.View
import Element exposing (Element)
import Element.Font
import Farm.Types exposing (..)
import Helpers.Element as EH exposing (DisplayProfile)
import Theme


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
            case model.userBalanceInfo of
                Nothing ->
                    Element.el
                        [ Element.centerX
                        , Element.centerY
                        , Element.Font.italic
                        , Element.Font.color Theme.darkGray
                        ]
                        (Element.text "Fetching info...")

                Just userBalanceInfo ->
                    Element.text "wut"
