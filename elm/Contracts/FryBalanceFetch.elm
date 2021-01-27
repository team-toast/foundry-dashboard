module Contracts.FryBalanceFetch exposing (..)

import AddressDict exposing (AddressDict)
import Config
import Contracts.Generated.ERC20BalanceFetchBatch as Generated
import Eth
import Eth.Types exposing (..)
import Http
import Task
import TokenValue exposing (TokenValue)


fetch :
    List Address
    -> (Result Http.Error (AddressDict TokenValue) -> msg)
    -> Cmd msg
fetch addresses msgConstructor =
    Eth.call
        Config.httpProviderUrl
        (Generated.balances Config.erc20BalanceFetchBatchContractAddress
            addresses
            [ Config.fryContractAddress ]
        )
        |> Task.attempt
            (Result.map
                (List.map TokenValue.tokenValue
                    >> List.map2 Tuple.pair addresses
                    >> AddressDict.fromList
                )
                >> msgConstructor
            )
