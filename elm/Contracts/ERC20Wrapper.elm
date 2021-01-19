module Contracts.ERC20Wrapper exposing (..)

--import Task

import AddressDict exposing (AddressDict)
import BigInt exposing (BigInt)
import Config exposing (httpProviderUrl)
import Contracts.Generated.ERC20 as ERC20
import Contracts.Generated.ERC20BalanceFetchBatch as Generated
import Dict exposing (Dict)
import Eth
import Eth.Types exposing (..)
import Eth.Utils
import Http
import Task
import TokenValue exposing (TokenValue)


getBalanceCmd :
    Address
    -> Address
    -> (Result Http.Error TokenValue -> msg)
    -> Cmd msg
getBalanceCmd tokenAddress owner msgConstructor =
    Eth.call
        httpProviderUrl
        (ERC20.balanceOf
            tokenAddress
            owner
        )
        |> Task.attempt
            (Result.map TokenValue.tokenValue >> msgConstructor)


getTotalSupply :
    Address
    -> (Result Http.Error TokenValue -> msg)
    -> Cmd msg
getTotalSupply tokenAddress msgConstructor =
    Eth.call
        httpProviderUrl
        (ERC20.totalSupply
            tokenAddress
        )
        |> Task.attempt
            (Result.map TokenValue.tokenValue >> msgConstructor)


getEthBalance :
    Address
    -> (Result Http.Error TokenValue -> msg)
    -> Cmd msg
getEthBalance address msgConstructor =
    Eth.getBalance
        httpProviderUrl
        address
        |> Task.attempt
            (Result.map TokenValue.tokenValue >> msgConstructor)
