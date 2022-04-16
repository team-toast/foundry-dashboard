module Contracts.FryBalanceFetch exposing (..)

import AddressDict exposing (AddressDict)
import Config
import Contracts.Generated.ERC20BalanceFetchBatch as BulkBalanceFetch
import Eth
import Eth.Types exposing (..)
import Http
import Task
import TokenValue exposing (TokenValue)
import Types exposing (Chain)
import Eth.Net exposing (NetworkId(..))
import Types exposing (Msg)


fetch :
    List Address
    -> (Result Http.Error (AddressDict TokenValue) -> msg)
    -> HttpProvider
    -> Address
    -> List Address
    -> Cmd msg
fetch addresses msgConstructor ethNode balanceFetcher erc20List =
    Eth.call
        ethNode
        (BulkBalanceFetch.balances 
            balanceFetcher
            addresses
            erc20List
        )
    |> Task.attempt
        (Result.map
            (List.map TokenValue.tokenValue
                >> List.map2 Tuple.pair addresses
                >> AddressDict.fromList
            )
            >> msgConstructor
        )

fetchMultiChainMultiWallet :
    List Address
    -> (Result Http.Error (AddressDict TokenValue) -> msg)
    -> List (HttpProvider, Address, List Address)
    -> List (Cmd msg)
fetchMultiChainMultiWallet addresses msgConstructor balanceQueryTuples =
    balanceQueryTuples 
    |> List.map (\(httpProvider, addressChecker, erc20List) -> fetch addresses msgConstructor httpProvider addressChecker erc20List) 

quickFetch :
    List Address
    -> (Result Http.Error (AddressDict TokenValue) -> msg)
    -> List (Cmd msg)
quickFetch addresses msgConstructor =
    fetchMultiChainMultiWallet 
        addresses 
        msgConstructor
        [ (Config.ethereumProviderUrl, Config.ethErc20BalanceFetchBatchContractAddress, [Config.ethereumFryContractAddress]) 
        , (Config.arbitrumProviderUls, Config.arbiErc20BalanceFetchBatchContractAddress, [Config.arbitrumOneFryContractAddress, Config.arbitrumOneGFryContractAddress])
        ]