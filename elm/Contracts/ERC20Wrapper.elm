module Contracts.ERC20Wrapper exposing (..)

import Config exposing (httpProviderUrl)
import Contracts.Generated.ERC20 as ERC20
import Eth
import Eth.Types exposing (..)
import Http
import Task
import TokenValue exposing (TokenValue)


getBalanceCmd :
    Int
    -> Address
    -> Address
    -> (Result Http.Error TokenValue -> msg)
    -> Cmd msg
getBalanceCmd networkId tokenAddress owner msgConstructor =
    Eth.call
        (httpProviderUrl networkId)
        (ERC20.balanceOf
            tokenAddress
            owner
        )
        |> Task.attempt
            (Result.map TokenValue.tokenValue >> msgConstructor)


getTotalSupply :
    Int
    -> Address
    -> (Result Http.Error TokenValue -> msg)
    -> Cmd msg
getTotalSupply networkId tokenAddress msgConstructor =
    Eth.call
        (httpProviderUrl networkId)
        (ERC20.totalSupply
            tokenAddress
        )
        |> Task.attempt
            (Result.map TokenValue.tokenValue >> msgConstructor)


getEthBalance :
    Int
    -> Address
    -> (Result Http.Error TokenValue -> msg)
    -> Cmd msg
getEthBalance networkId address msgConstructor =
    Eth.getBalance
        (httpProviderUrl networkId)
        address
        |> Task.attempt
            (Result.map TokenValue.tokenValue >> msgConstructor)
