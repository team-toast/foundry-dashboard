module Contracts.FryBalanceFetch exposing (..)

import AddressDict exposing (AddressDict)
import Config
import Contracts.Generated.ERC20BalanceFetchBatch as BulkBalanceFetch
import Eth
import Eth.Types exposing (..)
import Http
import Task
import TokenValue exposing (TokenValue)
import Types exposing (BalanceSet, Chain)
import Eth.Net exposing (NetworkId(..))
import Types exposing (Msg)
import Dict exposing (Dict)
import List.Extra exposing (uniqueBy)
import Eth.Utils exposing (addressToString)
import Contracts.Generated.ERC20BalanceFetchBatch exposing (balances)

toMaybeAddressDict :
    AddressDict a
    -> AddressDict (Maybe a)
toMaybeAddressDict dict =
    dict |> AddressDict.map (\key balance -> Just balance)
    
fetch :
    List Address
    -> (Result Http.Error (BalanceSet) -> msg)
    -> HttpProvider
    -> Address
    -> Address
    -> Cmd msg
fetch addresses msgConstructor ethNode balanceFetcher erc20 =
    Eth.call
        ethNode 
        (BulkBalanceFetch.balances 
            balanceFetcher
            addresses
            [ erc20 ]
        )
    |> Task.attempt
        (
            Result.map
                (List.map TokenValue.tokenValue
                    >> List.map2 Tuple.pair addresses 
                    >> (\list ->  
                        { providerUrl = ethNode
                        , erc20 = erc20
                        , balances = AddressDict.fromList list |> toMaybeAddressDict
                        })
                )
            >> msgConstructor
        )

fetchMultiChainMultiWallet :
    List Address
    -> (Result Http.Error (BalanceSet) -> msg)
    -> List (HttpProvider, Address, Address)
    -> List (Cmd msg)
fetchMultiChainMultiWallet addresses msgConstructor balanceQueryTuples =
    balanceQueryTuples 
    |> List.map (\(httpProvider, addressChecker, erc20) -> fetch addresses msgConstructor httpProvider addressChecker erc20) 

quickFetch :
    List Address
    -> (Result Http.Error (BalanceSet) -> msg)
    -> List (Cmd msg)
quickFetch addresses msgConstructor =
    fetchMultiChainMultiWallet 
        addresses 
        msgConstructor
        [ (Config.ethereumProviderUrl, Config.ethErc20BalanceFetchBatchContractAddress, Config.ethereumFryContractAddress) 
        , (Config.arbitrumProviderUls, Config.arbiErc20BalanceFetchBatchContractAddress, Config.arbitrumOneFryContractAddress)
        , (Config.arbitrumProviderUls, Config.arbiErc20BalanceFetchBatchContractAddress, Config.arbitrumOneGFryContractAddress)
        ]

updateBalancesFromResponse :
    BalanceSet
    -> List (BalanceSet)
    -> List (BalanceSet)
updateBalancesFromResponse response list =
    list 
    |> List.filter (\balanceSet -> not (balanceSet.providerUrl == response.providerUrl) && (balanceSet.erc20 == response.erc20))
    |> List.append [ response ]

getFryAddresses :
    List (BalanceSet)
    -> List Address
getFryAddresses list =
    list 
    |> List.map (\balanceSet -> balanceSet.balances |> AddressDict.keys)
    |> List.concat
    |> uniqueBy addressToString

getFryAddressesAsEmptyDict :
    List (BalanceSet)
    -> AddressDict (Maybe TokenValue)
getFryAddressesAsEmptyDict list =
    getFryAddresses list
    |> List.map (\address -> (address, Just TokenValue.zero))
    |> AddressDict.fromList

addBalances :
    AddressDict (Maybe TokenValue)
    -> AddressDict (Maybe TokenValue)
    -> AddressDict (Maybe TokenValue)
addBalances balances1 balances2 =
    let
        balancesList1 =
            balances1 |> AddressDict.toList
        
        balancesList2 = 
            balances2 |> AddressDict.toList

        castTokenMaybe a =
            case a of
                Just tokenValue ->
                    tokenValue
                Nothing ->
                    TokenValue.zero

        addTokenValue a b =
            (castTokenMaybe a)
            |> TokenValue.add (castTokenMaybe b)

        addTuples a b =
            (a |> Tuple.first, addTokenValue (a |> Tuple.second) (b |> Tuple.second) |> Just)
    in
        List.map2 addTuples balancesList1 balancesList2
        |> AddressDict.fromList

unifyFryBalances :
    List (BalanceSet)
    -> AddressDict (Maybe TokenValue)
unifyFryBalances list =
    list 
    |> List.map .balances
    |> List.foldl addBalances AddressDict.empty