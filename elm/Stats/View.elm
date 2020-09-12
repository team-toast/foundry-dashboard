module Stats.View exposing (view)

import Common.Types exposing (..)
import Common.View exposing (..)
import Config exposing (etherscanBaseUrl)
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
                { url = etherscanBaseUrl ++ Eth.Utils.addressToString model.addressFryToken
                , label = Element.text (Eth.Utils.addressToString model.addressFryToken)
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
                    { url = etherscanBaseUrl ++ Eth.Utils.addressToString model.addressTreasury
                    , label = Element.text (Eth.Utils.addressToString model.addressTreasury)
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
                    { url = etherscanBaseUrl ++ Eth.Utils.addressToString model.addressBucketSale
                    , label = Element.text (Eth.Utils.addressToString model.addressBucketSale)
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
                    { url = etherscanBaseUrl ++ Eth.Utils.addressToString model.addressMultiSig
                    , label = Element.text (Eth.Utils.addressToString model.addressMultiSig)
                    }
            ]
        ]