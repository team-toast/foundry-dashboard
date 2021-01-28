module View.Stats exposing (view)

import Config
import Element exposing (Attribute, Element, centerX, column, el, fill, height, newTabLink, padding, spacing, text, width)
import Element.Background
import Element.Border
import Element.Font
import ElementHelpers as EH exposing (DisplayProfile(..), responsiveVal)
import Eth.Types exposing (Address)
import Eth.Utils
import Misc exposing (calcEffectivePricePerToken, getBucketRemainingTimeText, loadingText, maybeFloatMultiply)
import Theme
import TokenValue exposing (TokenValue)
import Types exposing (Model, Msg)
import View.Common exposing (..)


view : Model -> Element Msg
view model =
    let
        mainEl =
            case model.dProfile of
                Desktop ->
                    Element.row

                Mobile ->
                    column
    in
    mainEl
        [ padding 20
        , spacing 25
        , centerX
        ]
        [ statsIcon model
        , viewAddresses model.dProfile
        ]


viewAddresses :
    DisplayProfile
    -> Element Msg
viewAddresses dProfile =
    [ text "Foundry Addresses"
        |> el
            [ Element.Font.size 30
            , Element.Font.color EH.white
            ]
    , viewAddressAndLabel dProfile "FRY token" Config.fryContractAddress
    , viewAddressAndLabel dProfile "Treasury" Config.treasuryForwarderAddress
    , viewAddressAndLabel dProfile "Bucket sale" Config.bucketSaleAddress
    , viewAddressAndLabel dProfile "Multisig" Config.teamToastMultiSigAddress
    ]
        |> column
            ([ width fill
             , centerX
             , padding 10
             , spacing 10
             , height fill
             ]
                ++ Theme.mainContainerBackgroundAttributes
                ++ Theme.mainContainerBorderAttributes
            )


viewAddressAndLabel :
    DisplayProfile
    -> String
    -> Address
    -> Element Msg
viewAddressAndLabel dProfile label address =
    [ label
        ++ ": "
        |> text
        |> el
            [ Element.Font.color EH.white
            , Element.Font.size <|
                responsiveVal
                    dProfile
                    22
                    20
            ]
    , { url =
            Config.etherscanBaseUrl
                ++ (address
                        |> Eth.Utils.addressToString
                   )
      , label =
            text
                (address
                    |> Eth.Utils.addressToString
                )
      }
        |> newTabLink
            [ Element.Font.color Theme.lightBlue ]
        |> el
            [ Element.Font.color EH.white
            , Element.Font.size <|
                responsiveVal
                    dProfile
                    16
                    12
            ]
    ]
        |> column
            [ padding 5
            , spacing 10
            , height (Element.px 50)
            , width fill
            ]


statsIcon : Model -> Element Msg
statsIcon model =
    let
        dProfile =
            model.dProfile

        defaultPadding =
            responsiveVal dProfile 10 7
                |> padding

        rowBorderStyle =
            Theme.childContainerBackgroundAttributes
                ++ Theme.childContainerBorderAttributes
                ++ [ centerX
                   ]

        bucketNumberEl =
            statsItem
                dProfile
                [ defaultPadding ]
                [ "BUCKET #"
                , intTextOrLoadingText model.currentBucketId
                ]

        timeLeftEl =
            statsItem
                dProfile
                [ defaultPadding ]
                [ "TIME LEFT"
                , getBucketRemainingTimeText
                    model.currentBucketId
                    model.currentTime
                ]

        bucketPriceEl =
            statsItem
                dProfile
                [ defaultPadding ]
                [ "BUCKET $"
                , "$ "
                    ++ calcEffectivePricePerToken
                        model.currentBucketTotalEntered
                        model.currentDaiPriceEth
                        model.currentEthPriceUsd
                ]

        uniswapPriceEl =
            statsItem
                dProfile
                [ defaultPadding ]
                [ "UNISWAP $"
                , "$ "
                    ++ (maybeFloatMultiply
                            model.currentFryPriceEth
                            model.currentEthPriceUsd
                            |> floatTextOrLoadingText
                       )
                ]

        circSupplyEl =
            statsItem
                dProfile
                [ defaultPadding ]
                [ "CIRC SUPPLY"
                , model.circSupply
                    |> floatTextOrLoadingText
                ]

        permaFrostEl =
            statsItem
                dProfile
                [ defaultPadding ]
                [ "PERMA-FROSTED"
                , model.permaFrostedTokens
                    |> tokenValueTextOrLoadingText
                ]

        totalSupplyEl =
            statsItem
                dProfile
                [ defaultPadding ]
                [ "TOTAL SUPPLY"
                , Config.fryTotalSupply
                    |> Just
                    |> intTextOrLoadingText
                ]

        marketCapEl =
            statsItem
                dProfile
                [ defaultPadding ]
                [ "MARKET CAP"
                , "$ "
                    ++ (model.marketCap
                            |> floatTextOrLoadingText
                       )
                ]

        fullyDilutedEl =
            statsItem
                dProfile
                [ defaultPadding ]
                [ "FULLY DILUTED"
                , "$ "
                    ++ (model.fullyDiluted
                            |> floatTextOrLoadingText
                       )
                ]

        treasuryBalanceEl =
            statsItem
                dProfile
                [ defaultPadding ]
                [ "TREASURY BALANCE"
                , "$ "
                    ++ (model.treasuryBalance
                            |> tokenValueTextOrLoadingText
                       )
                ]
    in
    column
        [ Element.Background.color <| Element.rgba 1 1 1 0.1
        , width fill
        , Element.Border.rounded 20
        , Element.Font.color Theme.lightGray
        , Element.Border.glow EH.white 2
        , padding 5
        , spacing 5
        ]
        [ Element.row
            rowBorderStyle
            [ bucketNumberEl
            , timeLeftEl
            ]
        , Element.row
            rowBorderStyle
            [ bucketPriceEl
            , uniswapPriceEl
            ]
        , Element.row
            rowBorderStyle
            [ marketCapEl
            , fullyDilutedEl
            ]
        , Element.row
            rowBorderStyle
            [ circSupplyEl
            , permaFrostEl
            , totalSupplyEl
            ]
        , Element.row
            rowBorderStyle
            [ treasuryBalanceEl ]
        ]


tokenValueTextOrLoadingText :
    Maybe TokenValue
    -> String
tokenValueTextOrLoadingText dilutedValue =
    case dilutedValue of
        Just val ->
            val
                |> TokenValue.toConciseString

        _ ->
            loadingText


floatTextOrLoadingText :
    Maybe Float
    -> String
floatTextOrLoadingText dilutedValue =
    case dilutedValue of
        Just val ->
            val
                |> TokenValue.fromFloatWithWarning
                |> TokenValue.toConciseString

        _ ->
            loadingText


intTextOrLoadingText :
    Maybe Int
    -> String
intTextOrLoadingText dilutedValue =
    case dilutedValue of
        Just val ->
            val
                |> toFloat
                |> TokenValue.fromFloatWithWarning
                |> TokenValue.toConciseString

        _ ->
            loadingText


textLarge :
    DisplayProfile
    -> String
    -> Element Msg
textLarge dProfile txt =
    el
        [ Element.Font.size <| responsiveVal dProfile 16 14
        , padding <| responsiveVal dProfile 5 2
        ]
    <|
        text txt


statsItem :
    DisplayProfile
    -> List (Attribute Msg)
    -> List String
    -> Element Msg
statsItem dProfile attributes items =
    items
        |> List.map (textLarge dProfile)
        |> column
            attributes
