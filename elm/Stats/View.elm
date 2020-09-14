module Stats.View exposing (view)

import Common.Types exposing (..)
import Common.View exposing (..)
import Config
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Element, height, padding, px, spacing, width)
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
        [ Element.row []
            [ Element.el
                [ Element.Font.color EH.white, Element.Font.size 30 ]
              <|
                Element.text " Foundry Addresses"
            ]
        , Element.row [ padding 5, spacing 5, height (px 50) ]
            [ Element.el
                [ Element.Font.color EH.white ]
              <|
                Element.text "FRY token: "
            , Element.el
                [ Element.Font.color EH.white ]
              <|
                Element.newTabLink []
                    { url = Config.etherscanBaseUrl ++ Eth.Utils.addressToString Config.fryTokenAddress
                    , label = Element.text (Eth.Utils.addressToString Config.fryTokenAddress)
                    }
            ]
        , Element.row [ padding 5, spacing 5, height (px 50) ]
            [ Element.el
                [ Element.Font.color EH.white ]
              <|
                Element.text "Treasury: "
            , Element.el
                [ Element.Font.color EH.white ]
              <|
                Element.newTabLink []
                    { url = Config.etherscanBaseUrl ++ Eth.Utils.addressToString Config.treasuryForwarderAddress
                    , label = Element.text (Eth.Utils.addressToString Config.treasuryForwarderAddress)
                    }
            ]
        , Element.row [ padding 5, spacing 5, height (px 50) ]
            [ Element.el
                [ Element.Font.color EH.white ]
              <|
                Element.text "Bucket sale: "
            , Element.el
                [ Element.Font.color EH.white ]
              <|
                Element.newTabLink []
                    { url = Config.etherscanBaseUrl ++ Eth.Utils.addressToString Config.bucketSaleAddress
                    , label = Element.text (Eth.Utils.addressToString Config.bucketSaleAddress)
                    }
            ]
        , Element.row [ padding 5, spacing 5, height (px 50) ]
            [ Element.el
                [ Element.Font.color EH.white ]
              <|
                Element.text "Multisig: "
            , Element.el
                [ Element.Font.color EH.white ]
              <|
                Element.newTabLink []
                    { url = Config.etherscanBaseUrl ++ Eth.Utils.addressToString Config.teamToastMultiSigAddress
                    , label = Element.text (Eth.Utils.addressToString Config.teamToastMultiSigAddress)
                    }
            ]
        ]
