module Contracts.Staking exposing (..)

import Config
import Contracts.Generated.ERC20 as ERC20
import Contracts.Generated.StakingRewards as StakingContract
import Eth
import Eth.Types exposing (..)
import Helpers.Eth as EthHelpers
import Http
import Task
import TokenValue exposing (TokenValue)


callStake : TokenValue -> (Result Http.Error () -> msg) -> Cmd msg
callStake amount msgConstructor =
    Eth.call
        Config.httpProviderUrl
        (StakingContract.stake
            Config.stakingContractAddress
            (TokenValue.getEvmValue amount)
        )
        |> Task.attempt msgConstructor


callGetReward : (Result Http.Error () -> msg) -> Cmd msg
callGetReward msgConstructor =
    Eth.call
        Config.httpProviderUrl
        (StakingContract.getReward
            Config.stakingContractAddress
        )
        |> Task.attempt msgConstructor


callExit : (Result Http.Error () -> msg) -> Cmd msg
callExit msgConstructor =
    Eth.call
        Config.httpProviderUrl
        (StakingContract.exit
            Config.stakingContractAddress
        )
        |> Task.attempt msgConstructor


getUnstakedBalance : Address -> (Result Http.Error TokenValue -> msg) -> Cmd msg
getUnstakedBalance address msgConstructor =
    Eth.call
        Config.httpProviderUrl
        (ERC20.balanceOf
            Config.stakingLiquidityContractAddress
            address
        )
        |> Task.attempt
            (Result.map TokenValue.tokenValue >> msgConstructor)


getStakedBalance : Address -> (Result Http.Error TokenValue -> msg) -> Cmd msg
getStakedBalance address msgConstructor =
    Eth.call
        Config.httpProviderUrl
        (ERC20.balanceOf
            Config.stakingContractAddress
            address
        )
        |> Task.attempt
            (Result.map TokenValue.tokenValue >> msgConstructor)


getAmountEarned : Address -> (Result Http.Error TokenValue -> msg) -> Cmd msg
getAmountEarned address msgConstructor =
    Eth.call
        Config.httpProviderUrl
        (StakingContract.earned
            Config.stakingContractAddress
            address
        )
        |> Task.attempt
            (Result.map TokenValue.tokenValue >> msgConstructor)
