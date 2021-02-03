module View.Stats exposing (view)

import Config
import Element exposing (Element, alignRight, centerX, column, el, fill, height, newTabLink, padding, paddingEach, px, row, spaceEvenly, spacing, spacingXY, text, width)
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
            , [ statsRowItem dProfile "Bucket $" bucketPrice
              , statsRowItem dProfile "UniSwap $" uniswapPrice
              ]
                |> statsRow
            , "Supply"
                |> statsHeading dProfile
            , [ statsRowItem dProfile "Circulating" circulatingSupply
              , statsRowItem dProfile "Total" totalSupply
              ]
                |> statsRow
            , "Market Cap"
                |> statsHeading dProfile
            , [ statsRowItem dProfile "Circulating" marketCap
              , statsRowItem dProfile "Fully Diluted" fullyDilutedMarketCap
              ]
                |> statsRow
            , "Sale"
                |> statsHeading dProfile
            , [ statsRowItem dProfile "Bucket #" bucketNumber
              , statsRowItem dProfile "Time Left" timeLeft
              ]
                |> statsRow
            , "Treasury"
                |> statsHeading dProfile
            , [ statsRowItem dProfile "Balance $" treasuryBalance ]
                |> statsRow
            , "Liquidity"
                |> statsHeading dProfile
            , [ statsRowItem dProfile "Permafrost $" permafrostDollars
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
              , [ statsRowItem dProfile "Bucket $" bucketPrice
                , statsRowItem dProfile "UniSwap $" uniswapPrice
                ]
                    |> statsRow
              , "Supply"
                    |> statsHeading dProfile
              , [ statsRowItem dProfile "Circulating" circulatingSupply
                , statsRowItem dProfile "Total" totalSupply
                ]
                    |> statsRow
              , "Market Cap"
                    |> statsHeading dProfile
              , [ statsRowItem dProfile "Circulating" marketCap
                , statsRowItem dProfile "Fully Diluted" fullyDilutedMarketCap
                ]
                    |> statsRow
              ]
                |> column
                    [ width (300 |> px)
                    , Element.Font.color Theme.lightGray
                    , padding 10
                    , spacing 10
                    ]
            , [ "Sale"
                    |> statsHeading dProfile
              , [ statsRowItem dProfile "Bucket #" bucketNumber
                , statsRowItem dProfile "Time Left" timeLeft
                ]
                    |> statsRow
              , "Treasury"
                    |> statsHeading dProfile
              , [ statsRowItem dProfile "Balance $" treasuryBalance ]
                    |> statsRow
              , "Liquidity"
                    |> statsHeading dProfile
              , [ statsRowItem dProfile "Permafrost $" permafrostDollars
                ]
                    |> statsRow
              ]
                |> column
                    [ width (300 |> px)
                    , Element.Font.color Theme.lightGray
                    , padding 10
                    , spacing 10
                    ]
            ]
                |> row
                    ([ width fill
                     , spacingXY 15 0
                     ]
                        ++ Theme.mainContainerBackgroundAttributes
                        ++ Theme.mainContainerBorderAttributes
                    )


statsRowItem : DisplayProfile -> String -> String -> Element Msg
statsRowItem dProfile label value =
    [ label
        |> textLarger dProfile
    , value
        |> textLarge dProfile
        |> el
            [ alignRight, Element.Font.color Theme.lightGray ]
    ]
        |> column [ Element.Font.color Theme.almostWhite ]


statsRow : List (Element Msg) -> Element Msg
statsRow items =
    [ items
        |> row
            ([ width fill
             , spaceEvenly
             , padding 5
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
            [ responsiveVal dProfile 16 14
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
