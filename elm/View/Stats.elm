module View.Stats exposing (view)

import BigInt
import Chain
import Config
import Element exposing (Element, alignRight, alignTop, centerX, column, el, fill, height, image, newTabLink, padding, paddingEach, px, row, spaceEvenly, spacing, spacingXY, text, width)
import Element.Border
import Element.Font
import ElementHelpers as EH exposing (DisplayProfile(..), responsiveVal)
import Eth.Types exposing (Address)
import Eth.Utils
import Helpers.Time as TimeHelpers
import Misc exposing (calcEffectivePricePerToken, calcPermaFrostedTokens, calcPermafrostedTokensValue, combineTreasuryBalance, getBucketRemainingTimeText, loadingText, maybeFloatMultiply)
import Theme
import Time
import TokenValue exposing (TokenValue)
import Types exposing (Chain(..), Model, Msg)
import View.Common exposing (..)
import Wallet


view : Model -> Element Msg
view model =
    let
        chain =
            model.wallet
                |> Wallet.getChainDefaultEth
    in
    Element.column
        [ centerX
        , padding 20
        , spacing 25
        ]
        [ Element.el [ centerX ] (weeksRemainingEl model.now)
        , (case model.dProfile of
            Desktop ->
                Element.row

            Mobile ->
                column
          )
            [ spacing 25
            , centerX
            ]
            [ statsEl model
            , viewAddresses chain model.dProfile
            ]
        ]


weeksRemainingEl : Time.Posix -> Element Msg
weeksRemainingEl now =
    let
        numWeeksStr =
            TimeHelpers.sub Config.saleEndTime now
                |> TimeHelpers.posixToSecondsBigInt
                |> (\a ->
                        BigInt.div a (BigInt.fromInt <| 60 * 60 * 24 * 7)
                            |> BigInt.toString
                   )
    in
    Element.column
        [ Element.Font.color EH.white
        , Element.spacing 5
        , Element.padding 15
        , Element.Border.width 1
        , Element.Border.rounded 7
        , Element.Border.color (Element.rgb 1 0 0)
        ]
        [ Element.el
            [ Element.centerX
            , Element.Font.size 50
            , Element.Font.bold
            ]
          <|
            Element.text <|
                numWeeksStr
                    ++ " WEEKS"
        , Element.el [ Element.Font.size 24 ] <| Element.text "to Foundry Sovereignty"
        ]


viewAddresses :
    Chain
    -> DisplayProfile
    -> Element Msg
viewAddresses chain dProfile =
    [ text "Foundry Addresses"
        |> el
            [ Element.Font.size 30
            , Element.Font.color EH.white
            ]
    , viewAddressAndLabel dProfile chain "FRY token" Config.ethereumFryContractAddress
    , case chain of
        Eth ->
            viewAddressAndLabel dProfile chain "Treasury" Config.mainTreasuryAddress

        _ ->
            Element.none
    , case chain of
        Eth ->
            viewAddressAndLabel dProfile chain "Bucket sale" Config.bucketSaleAddress

        _ ->
            Element.none
    , viewAddressAndLabel dProfile chain "Multisig" Config.teamToastMultiSigAddress
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
    -> Chain
    -> String
    -> Address
    -> Element Msg
viewAddressAndLabel dProfile chain label address =
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
            Config.blockExplorerUrl chain
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

        composedTreasuryBalance =
            "$ "
                ++ (model.composedTreasuryBalance
                        |> combineTreasuryBalance
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
            , [ statsRowItem dProfile "Balance" composedTreasuryBalance False
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
              , [ statsRowItem dProfile "Balance" composedTreasuryBalance False
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
