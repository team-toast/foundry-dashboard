module Contracts.Dai exposing (..)

import Config
import Contracts.Generated.ERC20 as ERC20
import Eth
import Eth.Types exposing (..)
import Http
import Task
import TokenValue exposing (TokenValue)


getBalanceCmd : Address -> (Result Http.Error TokenValue -> msg) -> Cmd msg
getBalanceCmd owner msgConstructor =
    Eth.call
        Config.httpProviderUrl
        (ERC20.balanceOf
            Config.daiContractAddress
            owner
        )
        |> Task.attempt
            (Result.map TokenValue.tokenValue >> msgConstructor)
