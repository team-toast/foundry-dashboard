module Stats.View exposing (view)

import Common.Types exposing (..)
import Common.View exposing (..)
import Config
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Attribute, Element)
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
        [ Element.padding 20
        , Element.spacing 25
        , Element.centerX
        ]
        [ statsIcon dProfile model
        , viewAddresses dProfile
        ]


viewAddresses :
    DisplayProfile
    -> Element Msg
viewAddresses dProfile =
    Element.column
        [ Element.Background.color <| Element.rgba 1 1 1 0.1
        , Element.width Element.fill
        , Element.Border.rounded 20
        , Element.Font.color EH.lightGray
        , Element.Border.glow EH.white 2
        , Element.centerX
        , Element.padding 10
        , Element.spacing 10
        ]
        [ Element.el
            [ Element.Font.size 30
            ]
          <|
            Element.text "Foundry Addresses"
        , viewAddressAndLabel dProfile "FRY token" Config.fryContractAddress
        , viewAddressAndLabel dProfile "Treasury" Config.treasuryForwarderAddress
        , viewAddressAndLabel dProfile "Bucket sale" Config.bucketSaleAddress
        , viewAddressAndLabel dProfile "Multisig" Config.teamToastMultiSigAddress
        ]


viewAddressAndLabel :
    DisplayProfile
    -> String
    -> Address
    -> Element Msg
viewAddressAndLabel dProfile label address =
    let
        mainEl =
            case dProfile of
                Desktop ->
                    Element.row

                Mobile ->
                    Element.column
    in
    mainEl
        [ Element.padding 5
        , Element.spacing 10
        , Element.height (Element.px 50)
        , Element.width Element.fill
        ]
        [ Element.el
            [ Element.Font.color EH.white
            , Element.Font.size <|
                responsiveVal
                    dProfile
                    22
                    20
            ]
          <|
            Element.text <|
                label
                    ++ ": "
        , Element.el
            [ Element.Font.color EH.white
            , responsiveVal dProfile Element.alignRight Element.alignLeft
            , Element.Font.size <|
                responsiveVal
                    dProfile
                    20
                    12
            ]
          <|
            Element.newTabLink [ Element.Font.color Theme.lightBlue ]
                { url = Config.etherscanBaseUrl ++ Eth.Utils.addressToString address
                , label = Element.text (Eth.Utils.addressToString address)
                }
        ]


statsIcon :
    DisplayProfile
    -> Model
    -> Element Msg
statsIcon dProfile model =
    let
        mainEl =
            case dProfile of
                Desktop ->
                    Element.row

                Mobile ->
                    Element.column

        defaultPadding =
            Element.padding <| responsiveVal dProfile 10 5
    in
    Element.column
        [ Element.Background.color <| Element.rgba 1 1 1 0.1
        , Element.width Element.fill
        , Element.Border.rounded 20
        , Element.Font.color EH.lightGray
        , Element.Border.glow EH.white 2
        ]
        [ mainEl
            [ Element.centerX
            ]
            [ columnItem
                dProfile
                [ defaultPadding ]
                [ "BUCKET #"
                , intTextOrLoadingText model.currentBucketId
                ]
            , columnItem
                dProfile
                [ defaultPadding ]
                [ "TIME LEFT"
                , getBucketRemainingTimeText
                    model.currentBucketId
                    model.currentTime
                ]
            , columnItem
                dProfile
                [ defaultPadding ]
                [ "CURRENT BUCKET"
                , "$ "
                    ++ calcEffectivePricePerToken
                        model.currentBucketTotalEntered
                        model.currentDaiPriceEth
                        model.currentEthPriceUsd
                ]
            , columnItem
                dProfile
                [ defaultPadding ]
                [ "UNISWAP"
                , "$ "
                    ++ (maybeFloatMultiply
                            model.currentFryPriceEth
                            model.currentEthPriceUsd
                            |> valueTextOrLoadingText
                       )
                ]
            , columnItem
                dProfile
                [ defaultPadding ]
                [ "MARKET CAP"
                , "$ "
                    ++ (model.marketCap
                            |> valueTextOrLoadingText
                       )
                ]
            , columnItem
                dProfile
                [ defaultPadding ]
                [ "FULLY DILUTED M/CAP"
                , "$ "
                    ++ (model.fullyDiluted
                            |> valueTextOrLoadingText
                       )
                ]
            , columnItem
                dProfile
                [ defaultPadding ]
                [ "CIRCULATING SUPPLY"
                , "$FRY "
                    ++ (model.circSupply
                            |> valueTextOrLoadingText
                       )
                ]
            ]
        ]


valueTextOrLoadingText : Maybe Float -> String
valueTextOrLoadingText dilutedValue =
    case dilutedValue of
        Just val ->
            val
                |> TokenValue.fromFloatWithWarning
                |> TokenValue.toConciseString

        _ ->
            loadingText


intTextOrLoadingText : Maybe Int -> String
intTextOrLoadingText dilutedValue =
    case dilutedValue of
        Just val ->
            val
                |> String.fromInt

        _ ->
            loadingText


textLarge :
    DisplayProfile
    -> String
    -> Element Msg
textLarge dProfile txt =
    Element.el
        [ Element.Font.size <| responsiveVal dProfile 16 14
        , Element.padding <| responsiveVal dProfile 5 2
        ]
    <|
        Element.text txt


columnItem :
    DisplayProfile
    -> List (Attribute Msg)
    -> List String
    -> Element Msg
columnItem dProfile attributes items =
    items
        |> List.map (textLarge dProfile)
        |> Element.column
            attributes
