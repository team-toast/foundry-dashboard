module Stats.View exposing (view)

import Common.Types exposing (..)
import Common.View exposing (..)
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
import Helpers.Element as EH exposing (DisplayProfile(..), changeForMobile, responsiveVal)
import Helpers.Tuple as TupleHelpers
import Routing exposing (Route)
import Stats.Types exposing (..)
import Theme exposing (darkTheme, defaultTheme)
import TokenValue exposing (TokenValue)
import Wallet exposing (Wallet)


view : EH.DisplayProfile -> Model -> Maybe UserInfo -> Element Msg
view dProfile model maybeUserInfo =
     Element.column []
        [
            Element.el
                [ Element.Font.color EH.white ] 
                <| Element.text ("FRY token address: " ++ (Eth.Utils.addressToString(model.addressFryToken)))
            ,
            Element.el 
                [ Element.Font.color EH.white ]
                <| Element.text ("Treasury address: " ++ (Eth.Utils.addressToString(model.addressTreasury)))
        ]
    
    
