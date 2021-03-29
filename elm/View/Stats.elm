module View.Stats exposing (view)

import Config
import Element exposing (Element, alignRight, alignTop, centerX, column, el, fill, height, image, newTabLink, padding, paddingEach, px, row, spaceEvenly, spacing, spacingXY, text, width)
import Element.Font
import ElementHelpers as EH exposing (DisplayProfile(..), responsiveVal)
import Eth.Types exposing (Address)
import Eth.Utils
import Misc exposing (calcEffectivePricePerToken, calcPermaFrostedTokens, calcPermafrostedTokensValue, getBucketRemainingTimeText, loadingText, maybeFloatMultiply)
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
        [ statsEl model
        , viewAddresses (Maybe.withDefault 0 model.networkId) model.dProfile
        ]


viewAddresses :
    Int
    -> DisplayProfile
    -> Element Msg
viewAddresses networkId dProfile =
    [ text "Foundry Addresses"
        |> el
            [ Element.Font.size 30
            , Element.Font.color EH.white
            ]
    , viewAddressAndLabel dProfile "FRY token" (Config.fryContractAddress networkId)
    , viewAddressAndLabel dProfile "Treasury" Config.treasuryForwarderAddress
    , viewAddressAndLabel dProfile "Bucket sale" Config.bucketSaleAddress
    , viewAddressAndLabel dProfile "Multisig" Config.teamToastMultiSigAddress
    ]
        |> column
            ([ width fill
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


statsEl : Model -> Element Msg
statsEl model =
    let
        dProfile =
            model.dProfile

        bucketNumber =
            intTextOrLoadingText model.currentBucketId

        uniswapPrice =
            "$ "
                ++ (maybeFloatMultiply
                        model.currentFryPriceEth
                        model.currentEthPriceUsd
                        |> floatTextOrLoadingText
                   )

        bucketPrice =
            "$ "
                ++ calcEffectivePricePerToken
                    model.currentBucketTotalEntered
                    model.currentDaiPriceEth
                    model.currentEthPriceUsd

        timeLeft =
            getBucketRemainingTimeText
                model.currentBucketId
                model.currentTime

        circulatingSupply =
            model.circSupply
                |> floatTextOrLoadingText

        totalSupply =
            Config.fryTotalSupply
                |> Just
                |> intTextOrLoadingText

        marketCap =
            "$ "
                ++ (model.marketCap
                        |> floatTextOrLoadingText
                   )

        fullyDilutedMarketCap =
            "$ "
                ++ (model.fullyDiluted
                        |> floatTextOrLoadingText
                   )

        treasuryBalance =
            "$ "
                ++ (model.treasuryBalance
                        |> tokenValueTextOrLoadingText
                   )

        permafrostDollars =
            "$ "
                ++ (calcPermafrostedTokensValue
                        model.permaFrostedTokens
                        model.currentFryPriceEth
                        model.currentEthPriceUsd
                        |> tokenValueTextOrLoadingText
                   )

        permafrostedTokens =
            model.permaFrostedTokens
                |> tokenValueTextOrLoadingText
    in
    case dProfile of
        Mobile ->
            [ "Price"
                |> statsHeading dProfile
            , [ statsRowItem dProfile "Bucket" bucketPrice False
              , statsRowItem dProfile "UniSwap" uniswapPrice False
              ]
                |> statsRow
            , "Supply"
                |> statsHeading dProfile
            , [ statsRowItem dProfile "Circulating" circulatingSupply True
              , statsRowItem dProfile "Total" totalSupply True
              ]
                |> statsRow
            , "Market Cap"
                |> statsHeading dProfile
            , [ statsRowItem dProfile "Circulating" marketCap False
              , statsRowItem dProfile "Fully Diluted" fullyDilutedMarketCap False
              ]
                |> statsRow
            , "Sale"
                |> statsHeading dProfile
            , [ statsRowItem dProfile "Bucket #" bucketNumber False
              , statsRowItem dProfile "Time Left" timeLeft False
              ]
                |> statsRow
            , "Treasury"
                |> statsHeading dProfile
            , [ statsRowItem dProfile "Balance" treasuryBalance False
              ]
                |> statsRow
            , "Liquidity"
                |> statsHeading dProfile
            , [ statsRowItem dProfile "Permafrost" permafrostDollars False
              ]
                |> statsRow
            ]
                |> column
                    ([ width fill
                     , Element.Font.color Theme.lightGray
                     , padding 10
                     , spacing 10
                     ]
                        ++ Theme.mainContainerBackgroundAttributes
                        ++ Theme.mainContainerBorderAttributes
                    )

        Desktop ->
            [ [ "Price"
                    |> statsHeading dProfile
              , [ statsRowItem dProfile "Bucket" bucketPrice False
                , statsRowItem dProfile "UniSwap" uniswapPrice False
                ]
                    |> statsRow
              , "Supply"
                    |> statsHeading dProfile
              , [ statsRowItem dProfile "Circulating" circulatingSupply True
                , statsRowItem dProfile "Total" totalSupply True
                ]
                    |> statsRow
              , "Market Cap"
                    |> statsHeading dProfile
              , [ statsRowItem dProfile "Circulating" marketCap False
                , statsRowItem dProfile "Fully Diluted" fullyDilutedMarketCap False
                ]
                    |> statsRow
              ]
                |> column
                    [ width (300 |> px)
                    , Element.Font.color Theme.lightGray
                    , padding 10
                    , spacing 10
                    , height fill
                    ]
            , [ "Sale"
                    |> statsHeading dProfile
              , [ statsRowItem dProfile "Bucket #" bucketNumber False
                , statsRowItem dProfile "Time Left" timeLeft False
                ]
                    |> statsRow
              , "Treasury"
                    |> statsHeading dProfile
              , [ statsRowItem dProfile "Balance" treasuryBalance False
                ]
                    |> statsRow
              , "Liquidity"
                    |> statsHeading dProfile
              , [ statsRowItem dProfile "Permafrost" permafrostDollars False
                ]
                    |> statsRow
              ]
                |> column
                    [ width (300 |> px)
                    , Element.Font.color Theme.lightGray
                    , padding 10
                    , spacing 10
                    , height fill
                    ]
            ]
                |> row
                    ([ width fill
                     , spacingXY 15 0
                     ]
                        ++ Theme.mainContainerBackgroundAttributes
                        ++ Theme.mainContainerBorderAttributes
                    )


statsRowItem : DisplayProfile -> String -> String -> Bool -> Element Msg
statsRowItem dProfile label value showFryIcon =
    [ if showFryIcon then
        { src = "img/poll-choice-mouseover.svg", description = "FRY Logo" }
            |> image
                [ 20
                    |> px
                    |> height
                ]

      else
        Element.none
    ]
        |> (++)
            [ label
                |> textLarge dProfile
            , value
                |> textLarge dProfile
                |> el
                    [ alignRight, Element.Font.color Theme.lightGray ]
            ]
        |> row
            [ Element.Font.color Theme.almostWhite
            , width fill
            ]


statsRow : List (Element Msg) -> Element Msg
statsRow items =
    [ items
        |> column
            ([ width fill
             , spaceEvenly
             , padding 5
             , height fill
             , alignTop
             ]
                ++ Theme.childContainerBorderAttributes
                ++ Theme.mainContainerBackgroundAttributes
            )
    ]
        |> row
            [ paddingEach
                { top = 5
                , left = 10
                , right = 0
                , bottom = 10
                }
            , width fill
            , height fill
            ]


statsHeading : DisplayProfile -> String -> Element Msg
statsHeading dProfile label =
    label
        |> text
        |> el
            [ responsiveVal dProfile 30 26
                |> Element.Font.size
            , Element.Font.color EH.white
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
    text txt
        |> el
            [ responsiveVal dProfile 18 14
                |> Element.Font.size
            , responsiveVal dProfile 5 2
                |> padding
            ]


textLarger :
    DisplayProfile
    -> String
    -> Element Msg
textLarger dProfile txt =
    text txt
        |> el
            [ responsiveVal dProfile 22 18
                |> Element.Font.size
            , responsiveVal dProfile 5 2
                |> padding
            ]
