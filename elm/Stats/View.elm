module Stats.View exposing (view)

import Common.Types exposing (..)
import Common.View exposing (..)
import Config
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Element, height, padding, px, spacing, width, fillPortion)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Eth.Types exposing (Address)
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..), changeForMobile, responsiveVal)
import Helpers.Tuple as TupleHelpers
import Routing exposing (Route)
import Stats.Types exposing (..)
import Theme exposing (darkTheme, defaultTheme)
import TokenValue exposing (TokenValue)
import Wallet exposing (Wallet)
import Element exposing (newTabLink)


view : EH.DisplayProfile -> Model -> Maybe UserInfo -> Element Msg
view dProfile model maybeUserInfo =
    Element.column
        [ padding 20 ]
        [ Element.row
            [ Element.width Element.fill ]
            [ Element.el
                [ Element.Font.color EH.white, Element.Font.size 30 ]
              <|
                Element.text " Foundry Addresses"
            ]
        , viewAddressAndLabel "FRY token" Config.fryTokenAddress
        , viewAddressAndLabel "Treasury" Config.treasuryForwarderAddress
        , viewAddressAndLabel "Bucket sale" Config.bucketSaleAddress
        , viewAddressAndLabel "Multisig" Config.teamToastMultiSigAddress
        , viewOtherUrlAndLabel "UniSwap" Config.uniswapPoolLink
        ]




labelElement: String -> Element Msg
labelElement string =
        Element.el
            [ Element.Font.color EH.white
            , Element.width (px 150) ]
          <|
            Element.text <|
                string
                    ++ ": "
        
newTabLinkElement: String -> String -> Element Msg
newTabLinkElement uri label =
        Element.el
            [ Element.Font.color EH.white ]
          <|
            Element.newTabLink [ Element.Font.color Theme.lightBlue ]
                { url = uri
                , label = Element.text label
                }


viewOtherUrlAndLabel: String -> String -> Element Msg
viewOtherUrlAndLabel label uri =
    Element.row
        [ padding 5
        , spacing 10
        , height (px 50)
        , Element.width Element.fill
        ]
        [ labelElement label                    
        , newTabLinkElement uri (reduceString uri)
        ]

reduceString: String -> String
reduceString string = 
    if String.length string > 47 then
        String.slice 0 44 string ++ "..."
    else
        string

viewAddressAndLabel : String -> Address -> Element Msg
viewAddressAndLabel label address =
    Element.row
        [ padding 5
        , spacing 10
        , height (px 50)
        , Element.width Element.fill
        ]
        [ labelElement label
        , newTabLinkElement 
            (Config.etherscanBaseUrl ++ Eth.Utils.addressToString address)
            (Eth.Utils.addressToString address)                
        ]
