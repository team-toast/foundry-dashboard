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
import Eth.Types exposing (Address)
import Eth.Utils
import Helpers.Element as EH exposing (DisplayProfile(..), responsiveVal)
import Helpers.Tuple as TupleHelpers
import Routing exposing (Route)
import Stats.Types exposing (..)
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
    Element.column
        [ padding 20 ]
        [ statsIcon model
        , Element.row
            [ Element.width Element.fill ]
            [ Element.el
                [ Element.Font.color EH.white
                , Element.Font.size 30
                ]
              <|
                Element.text " Foundry Addresses"
            ]
        , viewAddressAndLabel "FRY token" Config.fryContractAddress
        , viewAddressAndLabel "Treasury" Config.treasuryForwarderAddress
        , viewAddressAndLabel "Bucket sale" Config.bucketSaleAddress
        , viewAddressAndLabel "Multisig" Config.teamToastMultiSigAddress
        ]


viewAddressAndLabel :
    String
    -> Address
    -> Element Msg
viewAddressAndLabel label address =
    Element.row
        [ padding 5
        , spacing 10
        , height (px 50)
        , Element.width Element.fill
        ]
        [ Element.el
            [ Element.Font.color EH.white ]
          <|
            Element.text <|
                label
                    ++ ": "
        , Element.el
            [ Element.Font.color EH.white
            , Element.alignRight
            ]
          <|
            Element.newTabLink [ Element.Font.color Theme.lightBlue ]
                { url = Config.etherscanBaseUrl ++ Eth.Utils.addressToString address
                , label = Element.text (Eth.Utils.addressToString address)
                }
        ]


statsIcon :
    Model
    -> Element Msg
statsIcon model =
    Element.column
        [ Element.Background.color Theme.softRed
        , Element.width Element.fill
        , Element.Border.rounded 50
        ]
        [ Element.row
            [ Element.paddingEach
                { left = 15
                , top = 0
                , right = 0
                , bottom = 0
                }
            , Element.centerX
            ]
            [ Element.column
                [ Element.padding 20 ]
                [ "BUCKET #"
                    |> textLarge
                , String.fromInt model.currentBucketId
                    |> textLarge
                ]
            , Element.column
                [ Element.padding 20 ]
                [ "TIME LEFT"
                    |> textLarge
                , getBucketRemainingTimeText
                    model.currentBucketId
                    model.currentTime
                    |> textLarge
                ]
            , Element.column
                []
                [ Element.row
                    []
                    [ Element.column
                        [ Element.padding 5 ]
                        (columnItems
                            "SALE"
                            ("$ "
                                ++ (calcEffectivePricePerToken
                                        model.currentBucketTotalEntered
                                        ((if model.currentDaiPriceEth == 0 then
                                            1.01

                                          else
                                            model.currentDaiPriceEth
                                         )
                                            * model.currentEthPriceUsd
                                        )
                                        |> TokenValue.toConciseString
                                   )
                            )
                        )
                    , Element.column
                        [ Element.padding 5
                        , Element.alignRight
                        ]
                        (columnItems
                            "UNISWAP"
                            ("$ "
                                ++ (model.currentFryPriceEth
                                        * model.currentEthPriceUsd
                                        |> TokenValue.fromFloatWithWarning
                                        |> TokenValue.toConciseString
                                   )
                            )
                        )
                    ]
                ]
            , Element.column
                []
                [ Element.row
                    []
                    [ Element.column
                        [ Element.padding 5 ]
                        (columnItems
                            "MARKET CAP"
                            ("$ "
                                ++ (model.marketCap
                                        |> TokenValue.fromFloatWithWarning
                                        |> TokenValue.toConciseString
                                   )
                            )
                        )
                    ]
                ]
            ]
        ]


textLarge : String -> Element Msg
textLarge txt =
    Element.el
        [ Element.Font.size 16
        , Element.padding 5
        ]
    <|
        Element.text txt


columnItems : String -> String -> List (Element Msg)
columnItems str1 str2 =
    [ textLarge str1
    , textLarge str2
    ]
