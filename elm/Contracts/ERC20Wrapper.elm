module Contracts.ERC20Wrapper exposing (..)

import Config exposing (nodeUrl)
import Contracts.Generated.ERC20 as ERC20
import Eth
import Eth.Types exposing (..)
import Http
import Task
import TokenValue exposing (TokenValue)
import Types exposing (ChainConfigs, ChainId)


getBalanceCmd :
    String
    -> Address
    -> Address
    -> (Result Http.Error TokenValue -> msg)
    -> Cmd msg
getBalanceCmd nodeUrl tokenAddress owner msgConstructor =
    Eth.call
        nodeUrl
        (ERC20.balanceOf
            tokenAddress
            owner
        )
        |> Task.attempt
            (Result.map TokenValue.tokenValue >> msgConstructor)


getTotalSupply :
    String
    -> Address
    -> (Result Http.Error TokenValue -> msg)
    -> Cmd msg
getTotalSupply nodeUrl tokenAddress msgConstructor =
    Eth.call
        nodeUrl
        (ERC20.totalSupply
            tokenAddress
        )
        |> Task.attempt
            (Result.map TokenValue.tokenValue >> msgConstructor)


getEthBalance :
    String
    -> Address
    -> (Result Http.Error TokenValue -> msg)
    -> Cmd msg
getEthBalance nodeUrl address msgConstructor =
    Eth.getBalance
        nodeUrl
        address
        |> Task.attempt
            (Result.map TokenValue.tokenValue >> msgConstructor)
