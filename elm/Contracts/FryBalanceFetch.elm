module Contracts.FryBalanceFetch exposing (..)

import AddressDict exposing (..)
import BigInt exposing (BigInt)
import Config
import Contracts.Generated.ERC20BalanceFetchBatch as BulkBalanceFetch
import Contracts.Generated.LPBalancerChecker as LPBalancerChecker
import Dict
import Eth
import Eth.Net exposing (NetworkId(..))
import Eth.Types exposing (..)
import Eth.Utils exposing (..)
import Http
import List.Extra exposing (..)
import Task
import TokenValue exposing (TokenValue)
import Types exposing (ChainConfigs, TokenBalanceDict)


fetchLP :
    (Result Http.Error ( HttpProvider, Address, AddressDict TokenValue ) -> msg)
    -> HttpProvider
    -> Address
    -> Address
    -> Address
    -> Address
    -> List Address
    -> Cmd msg
fetchLP msgConstructor httpProvider batchLPReaderAddress batchCheckerAddress lpAddress tokenAddress addresses =
    let
        translateResult : List BigInt -> ( HttpProvider, Address, AddressDict TokenValue )
        translateResult result =
            result
                |> (List.map TokenValue.tokenValue
                        >> List.map2 Tuple.pair addresses
                        >> AddressDict.fromList
                   )
                >> (\i -> ( httpProvider, lpAddress, i ))
    in
    Eth.call
        httpProvider
        (LPBalancerChecker.getBalances
            batchLPReaderAddress
            batchCheckerAddress
            lpAddress
            tokenAddress
            addresses
        )
        |> Task.attempt
            (Result.map
                translateResult
                >> msgConstructor
            )


fetch :
    (Result Http.Error ( HttpProvider, Address, AddressDict TokenValue ) -> msg)
    -> HttpProvider
    -> Address
    -> Address
    -> List Address
    -> Cmd msg
fetch msgConstructor httpProvider batchErc20ReaderAddress erc20Address addresses =
    let
        translateResult : List BigInt -> ( HttpProvider, Address, AddressDict TokenValue )
        translateResult result =
            result
                |> (List.map TokenValue.tokenValue
                        >> List.map2 Tuple.pair addresses
                        >> AddressDict.fromList
                   )
                >> (\i -> ( httpProvider, erc20Address, i ))
    in
    Eth.call
        httpProvider
        (BulkBalanceFetch.balances batchErc20ReaderAddress
            addresses
            [ erc20Address ]
        )
        |> Task.attempt
            (Result.map
                translateResult
                >> msgConstructor
            )


accumulateFetches :
    (Result Http.Error ( HttpProvider, Address, AddressDict TokenValue ) -> msg)
    -> List Address
    -> List (Cmd msg)
accumulateFetches msgConstructor addresses =
    [ fetch
        msgConstructor
        Config.ethereumProviderUrl
        Config.ethErc20BalanceFetchBatchContractAddress
        Config.ethereumFryContractAddress
        addresses
    , fetchLP
        msgConstructor
        Config.ethereumProviderUrl
        Config.ethLPBalanceFetchBatchContractAddress
        Config.ethErc20BalanceFetchBatchContractAddress
        Config.ethBalancerFryDaiDethContractAddress
        Config.ethereumFryContractAddress
        addresses
    , fetchLP
        msgConstructor
        Config.ethereumProviderUrl
        Config.ethLPBalanceFetchBatchContractAddress
        Config.ethErc20BalanceFetchBatchContractAddress
        Config.ethBalancerWstaWethAaveUniYfiFryContractAddress
        Config.ethereumFryContractAddress
        addresses
    , fetchLP
        msgConstructor
        Config.ethereumProviderUrl
        Config.ethLPBalanceFetchBatchContractAddress
        Config.ethErc20BalanceFetchBatchContractAddress
        Config.ethUniswapFryWethContractAddress
        Config.ethereumFryContractAddress
        addresses
    , fetch
        msgConstructor
        Config.arbitrumProviderUrl
        Config.arbErc20BalanceFetchBatchContractAddress
        Config.arbitrumOneFryContractAddress
        addresses
    , fetch
        msgConstructor
        Config.arbitrumProviderUrl
        Config.arbErc20BalanceFetchBatchContractAddress
        Config.arbitrumOneGFryContractAddress
        addresses
    , fetch
        msgConstructor
        Config.polygonProviderUrl
        Config.polyErc20BalanceFetchBatchContractAddress
        Config.polyFryContractAddress
        addresses
    , fetch
        msgConstructor
        Config.polygonProviderUrl
        Config.polyErc20BalanceFetchBatchContractAddress
        Config.polyGFryContractAddress
        addresses
    , fetch
        msgConstructor
        Config.polygonProviderUrl
        Config.polyErc20BalanceFetchBatchContractAddress
        Config.polyFryContractAddress
        addresses
    ]


updateTokenBalanceDict :
    ( HttpProvider, Address, AddressDict TokenValue )
    -> TokenBalanceDict
    -> TokenBalanceDict
updateTokenBalanceDict newBalances dict =
    let
        ( _, erc20Address, newTokenValues ) =
            newBalances

        existingTokenValues =
            dict |> AddressDict.get erc20Address

        resultingUnion =
            case existingTokenValues of
                Nothing ->
                    newTokenValues |> AddressDict.map (\_ i -> Just i)

                Just a ->
                    a
                        |> AddressDict.union (newTokenValues |> AddressDict.map (\_ i -> Just i))
    in
    dict
        |> AddressDict.insert erc20Address resultingUnion


unifyFryBalances :
    TokenBalanceDict
    -> AddressDict (Maybe TokenValue)
unifyFryBalances fryBalances =
    let
        addMaybe : Maybe TokenValue -> Maybe TokenValue -> Maybe TokenValue
        addMaybe a b =
            case a of
                Nothing ->
                    b

                Just aVal ->
                    case b of
                        Nothing ->
                            Just aVal

                        Just bVal ->
                            Just (aVal |> TokenValue.add bVal)

        mergeTwo a b =
            AddressDict.merge
                AddressDict.insert
                (\k v1 v2 -> AddressDict.insert k (v1 |> addMaybe v2))
                AddressDict.insert
                a
                b
                AddressDict.empty
    in
    fryBalances
        |> AddressDict.toList
        |> List.map (\( _, v ) -> v)
        |> List.foldl mergeTwo AddressDict.empty
