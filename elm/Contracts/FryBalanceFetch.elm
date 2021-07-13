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


fetch :
    List Address
    -> (Result Http.Error (AddressDict TokenValue) -> msg)
    -> Cmd msg
fetch addresses msgConstructor =
    Eth.call
        (Config.ethereumProviderUrl)
        (BulkBalanceFetch.balances Config.erc20BalanceFetchBatchContractAddress
            addresses
            [ Config.ethereumFryContractAddress ]
        )
        |> Task.attempt
            (Result.map
                (List.map TokenValue.tokenValue
                    >> List.map2 Tuple.pair addresses
                    >> AddressDict.fromList
                )
                >> msgConstructor
            )
