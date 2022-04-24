module Contracts.ERC20Wrapper exposing (..)

import Config exposing (httpProviderUrl)
import Contracts.Generated.ERC20 as ERC20
import Eth
import Eth.Types exposing (..)
import Http
import Task
import TokenValue exposing (TokenValue)
import Types exposing (ChainId)


getBalanceCmd :
    ChainId
    -> Address
    -> Address
    -> (Result Http.Error TokenValue -> msg)
    -> Cmd msg
getBalanceCmd chain tokenAddress owner msgConstructor =
    Eth.call
        (httpProviderUrl chain)
        (ERC20.balanceOf
            tokenAddress
            owner
        )
        |> Task.attempt
            (Result.map TokenValue.tokenValue >> msgConstructor)


getTotalSupply :
    ChainId
    -> Address
    -> (Result Http.Error TokenValue -> msg)
    -> Cmd msg
getTotalSupply chain tokenAddress msgConstructor =
    Eth.call
        (httpProviderUrl chain)
        (ERC20.totalSupply
            tokenAddress
        )
        |> Task.attempt
            (Result.map TokenValue.tokenValue >> msgConstructor)


getEthBalance :
    ChainId
    -> Address
    -> (Result Http.Error TokenValue -> msg)
    -> Cmd msg
getEthBalance chain address msgConstructor =
    Eth.getBalance
        (httpProviderUrl chain)
        address
        |> Task.attempt
            (Result.map TokenValue.tokenValue >> msgConstructor)
