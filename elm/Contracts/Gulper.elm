module Contracts.Gulper exposing (..)

import Config
import Contracts.Generated.Gulper as Binding
import Eth
import Eth.Types exposing (Address)
import Http
import Task
import TokenValue exposing (TokenValue)


getTotalRaised : Address -> (Result Http.Error TokenValue -> msg) -> Cmd msg
getTotalRaised contractAddress msgConstructor =
    Eth.call
        Config.ethereumProviderUrl
        (Binding.totalRaised contractAddress)
        |> Task.map TokenValue.tokenValue
        |> Task.attempt msgConstructor
