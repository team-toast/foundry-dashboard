module View.Stats exposing (view)

import AddressDict
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
import List.Extra
import Misc exposing (calcEffectivePricePerToken, calcPermaFrostedTokens, calcPermafrostedTokensValue, combineTreasuryBalance, getBucketRemainingTimeText, loadingText, maybeFloatMultiply)
import Theme
import Time
import TokenValue exposing (TokenValue)
import Types exposing (ChainConfigs, ChainId, Model, Msg)
import View.Common exposing (..)
import Wallet


view : Model -> Element Msg
view model =
    let
        chainId =
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
            , viewAddresses chainId model.chainConfigs model.dProfile
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
    ChainId
    -> ChainConfigs
    -> DisplayProfile
    -> Element Msg
viewAddresses chainId chainConfigs dProfile =
    [ text "Foundry Addresses"
        |> el
            [ Element.Font.size 30
            , Element.Font.color EH.white
            ]
    , viewAddressAndLabel dProfile chainId chainConfigs "FRY token" Config.ethereumFryContractAddress
    , if chainId == 1 then
        viewAddressAndLabel dProfile chainId chainConfigs "Treasury" Config.mainTreasuryAddress

      else
        Element.none
    , if chainId == 1 then
        viewAddressAndLabel dProfile chainId chainConfigs "Bucket sale" Config.bucketSaleAddress

      else
        Element.none
    , viewAddressAndLabel dProfile chainId chainConfigs "Multisig" Config.teamToastMultiSigAddress
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
    -> ChainId
    -> ChainConfigs
    -> String
    -> Address
    -> Element Msg
viewAddressAndLabel dProfile chainId chainConfigs label address =
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
            Config.blockExplorerUrl chainId chainConfigs
                |> Maybe.map ((++) (Eth.Utils.addressToString address))
                |> Maybe.withDefault ""
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

        dethTVLString =
            model.dethTVL
                |> Maybe.map (\tvl -> TokenValue.div tvl 1000)
                |> Maybe.map TokenValue.toConciseString
                |> Maybe.map (\valStr -> "$" ++ valStr ++ "k")
                |> Maybe.withDefault loadingText

        dethNumUniqueMints =
            model.dethUniqueMints
                |> AddressDict.keys
                |> List.length

        maybeEthValString maybeEthVal =
            maybeEthVal
                |> Maybe.map TokenValue.toConciseString
                |> Maybe.map (\valStr -> valStr ++ " ETH")
                |> Maybe.withDefault loadingText

        maybeEthValAndConvertedParenthetical maybeEthVal =
            maybeEthValString maybeEthVal
                ++ (maybeConvertedEthValParenthetical maybeEthVal
                        |> Maybe.map (\s -> "  " ++ s)
                        |> Maybe.withDefault ""
                   )

        maybeConvertedEthValParenthetical maybeEthVal =
            Maybe.map2
                (\ethVal currentEthPriceUsd ->
                    TokenValue.mulFloatWithWarning
                        ethVal
                        currentEthPriceUsd
                )
                maybeEthVal
                model.currentEthPriceUsd
                |> Maybe.map TokenValue.toConciseString
                |> Maybe.map
                    (\numStr ->
                        "(~$" ++ numStr ++ ")"
                    )

        permafrostedTokens =
            model.permaFrostedTokens
                |> tokenValueTextOrLoadingText

        elementBlockList =
            let
                makeBlock title el =
                    Element.column
                        [ Element.spacing 10
                        , Element.width <| Element.px 300
                        ]
                        [ statsHeading dProfile title
                        , el
                        ]
            in
            [ makeBlock "Funds"
                ([ statsRowItem dProfile "Treasury" composedTreasuryBalance False
                 , statsRowItem dProfile "Permafrost" permafrostDollars False
                 ]
                    |> statsRow
                )
            , makeBlock "dETH"
                ([ statsRowItem dProfile
                    "Profit to date"
                    (maybeEthValAndConvertedParenthetical model.dethProfit)
                    False
                 , statsRowItem dProfile
                    "TVL"
                    dethTVLString
                    False

                 -- , statsRowItem dProfile
                 --     "Num"
                 --     (String.fromInt dethNumUniqueMints)
                 --     False
                 ]
                    |> statsRow
                )
            , makeBlock "Price"
                ([ statsRowItem dProfile "Bucket" bucketPrice False
                 , statsRowItem dProfile "UniSwap" uniswapPrice False
                 ]
                    |> statsRow
                )
            , makeBlock "Sale"
                ([ statsRowItem dProfile "Bucket #" bucketNumber False
                 , statsRowItem dProfile "Time Left" timeLeft False
                 ]
                    |> statsRow
                )
            , makeBlock "Market Cap"
                ([ statsRowItem dProfile "Circulating" marketCap False
                 , statsRowItem dProfile "Fully Diluted" fullyDilutedMarketCap False
                 ]
                    |> statsRow
                )
            , makeBlock "Supply"
                ([ statsRowItem dProfile "Circulating" circulatingSupply True
                 , statsRowItem dProfile "Total" totalSupply True
                 ]
                    |> statsRow
                )
            ]
    in
    case dProfile of
        Mobile ->
            column
                ([ width fill
                 , Element.Font.color Theme.lightGray
                 , padding 10
                 , spacing 10
                 ]
                    ++ Theme.mainContainerBackgroundAttributes
                    ++ Theme.mainContainerBorderAttributes
                )
                elementBlockList

        Desktop ->
            elementBlockList
                |> List.Extra.greedyGroupsOf 2
                |> List.map
                    (row
                        [ width fill
                        , spacingXY 15 0
                        ]
                    )
                |> column
                    ([ width Element.fill
                     , Element.Font.color Theme.lightGray
                     , padding 10
                     , spacing 10
                     , height fill
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
