module Contracts.FryBalanceFetch exposing (..)

import AddressDict exposing (AddressDict)
import BigInt exposing (BigInt)
import Config
import Contracts.Generated.ERC20BalanceFetchBatch as BulkBalanceFetch
import Eth
import Eth.Types exposing (..)
import Http
import Task
import TokenValue exposing (TokenValue)
import Eth.Net exposing (NetworkId(..))
import List.Extra exposing (uniqueBy)
import Eth.Utils exposing (addressToString)
import Types exposing (CrossChainFryBalanceTracker, Msg)

fetch :
    (Result Http.Error (HttpProvider, Address, AddressDict TokenValue) -> msg)
    -> HttpProvider
    -> Address
    -> Address
    -> List Address
    -> Cmd msg
fetch msgConstructor httpProvider batchErc20ReaderAddress erc20Address addresses =
    let
        translateResult : List BigInt -> (HttpProvider, Address, AddressDict TokenValue)
        translateResult result = 
            result 
            |> (
                List.map TokenValue.tokenValue
                >> List.map2 Tuple.pair addresses
                >> AddressDict.fromList
            )
            >> (\i -> (httpProvider, erc20Address, i))
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
    (Result Http.Error (HttpProvider, Address, AddressDict TokenValue) -> msg)
    -> AddressDict (CrossChainFryBalanceTracker)
    -> List (Cmd msg)
accumulateFetches msgConstructor dict =
    -- transform the dict into 3 lists of addresses 
    -- create a cmd to fetch each list
    let
        filterBalancesToList fn =
            dict
            |> AddressDict.map  (\_ i -> fn i)
            |> AddressDict.filter (\_ i -> Maybe.Nothing == i)
            |> AddressDict.toList 
            |> List.map (\(k, _) -> k)
    in
        [ filterBalancesToList .ethFryBalance 
            |> fetch 
                msgConstructor 
                Config.ethereumProviderUrl 
                Config.ethErc20BalanceFetchBatchContractAddress 
                Config.ethereumFryContractAddress
        , filterBalancesToList .arbFryBalance 
            |> fetch 
                msgConstructor 
                Config.arbitrumProviderUrl 
                Config.arbErc20BalanceFetchBatchContractAddress
                Config.arbitrumOneFryContractAddress
        , filterBalancesToList .arbGFryBalance 
            |> fetch 
                msgConstructor 
                Config.arbitrumProviderUrl 
                Config.arbErc20BalanceFetchBatchContractAddress
                Config.arbitrumOneGFryContractAddress ]


updateCrossChainFryBalanceTracker :
    (HttpProvider, Address, AddressDict TokenValue)
    -> AddressDict (CrossChainFryBalanceTracker)
    -> AddressDict (CrossChainFryBalanceTracker)
updateCrossChainFryBalanceTracker newBalances dict =
    let
        (_, erc20Address, balances) = newBalances
    in
        balances
        |>
            (if erc20Address == Config.ethereumFryContractAddress then
                AddressDict.map (\_ i -> CrossChainFryBalanceTracker (Just i) Nothing Nothing)
            else if erc20Address == Config.arbitrumOneFryContractAddress then
                AddressDict.map (\_ i -> CrossChainFryBalanceTracker Nothing (Just i) Nothing)
            else -- if erc20Address == Config.arbitrumOneGFryContractAddress then
                AddressDict.map (\_ i -> CrossChainFryBalanceTracker Nothing Nothing (Just i)))
        |> AddressDict.union dict

unifyFryBalances :
    AddressDict CrossChainFryBalanceTracker
    -> AddressDict (Maybe TokenValue)
unifyFryBalances fryBalances =
    let
        addMaybe : Maybe TokenValue -> Maybe TokenValue -> Maybe TokenValue
        addMaybe a b =
            case a of
                Nothing -> b
                Just aVal -> 
                    case b of
                        Nothing -> Just aVal
                        Just bVal -> Just (aVal |> TokenValue.add bVal)
    in
    fryBalances |>
    AddressDict.map (\_ i -> (i.ethFryBalance |> addMaybe i.arbGFryBalance |> addMaybe i.arbFryBalance))