module Contracts.Staking exposing (..)

import BigInt exposing (BigInt)
import Config
import Contracts.Generated.ERC20 as ERC20
import Contracts.Generated.StakingRewards as StakingContract
import Contracts.Generated.StakingScripts as StakingScripts
import Eth
import Eth.Types as Eth exposing (Address)
import Helpers.BigInt as BigIntHelpers
import Helpers.Eth as EthHelpers
import Helpers.Time as TimeHelpers
import Http
import Task
import TokenValue exposing (TokenValue)
import Types exposing (UserStakingInfo)


approveLiquidityToken : Int -> Eth.Send
approveLiquidityToken networkId =
    ERC20.approve
        (Config.stakingLiquidityContractAddress networkId)
        (Config.stakingContractAddress networkId)
        (TokenValue.maxTokenValue |> TokenValue.getEvmValue)
        |> Eth.toSend


stake : Int -> TokenValue -> Eth.Send
stake networkId amount =
    StakingContract.stake
        (Config.stakingContractAddress networkId)
        (TokenValue.getEvmValue amount)
        |> Eth.toSend


withdraw : Int -> TokenValue -> Eth.Send
withdraw networkId amount =
    StakingContract.withdraw
        (Config.stakingContractAddress networkId)
        (TokenValue.getEvmValue amount)
        |> Eth.toSend


exit : Int -> Eth.Send
exit networkId =
    StakingContract.exit
        (Config.stakingContractAddress networkId)
        |> Eth.toSend


claimRewards : Int -> Eth.Send
claimRewards networkId =
    StakingContract.getReward
        (Config.stakingContractAddress networkId)
        |> Eth.toSend



-- callWithdraw : TokenValue -> (Result Http.Error () -> msg) -> Cmd msg
-- callWithdraw amount msgConstructor =
--     Eth.call
--         Config.httpProviderUrl
--         (StakingContract.withdraw
--             Config.stakingContractAddress
--             (TokenValue.getEvmValue amount)
--         )
--         |> Task.attempt msgConstructor
-- callGetReward : (Result Http.Error () -> msg) -> Cmd msg
-- callGetReward msgConstructor =
--     Eth.call
--         Config.httpProviderUrl
--         (StakingContract.getReward
--             Config.stakingContractAddress
--         )
--         |> Task.attempt msgConstructor
-- callExit : (Result Http.Error () -> msg) -> Cmd msg
-- callExit msgConstructor =
--     Eth.call
--         Config.httpProviderUrl
--         (StakingContract.exit
--             Config.stakingContractAddress
--         )
--         |> Task.attempt msgConstructor


getUserStakingInfo : Int -> Address -> (Result Http.Error ( UserStakingInfo, Float ) -> msg) -> Cmd msg
getUserStakingInfo networkId userAddress msgConstructor =
    Eth.call
        (Config.httpProviderUrl networkId)
        (StakingScripts.getData
            (Config.stakingScriptsAddress networkId)
            (Config.stakingContractAddress networkId)
            (Config.stakingPricePairAddress networkId)
            userAddress
        )
        |> Task.map unpackBindingStruct
        |> Task.attempt msgConstructor


unpackBindingStruct : StakingScripts.GetData -> ( UserStakingInfo, Float )
unpackBindingStruct data =
    ( { unstaked = TokenValue.tokenValue data.availableBalance
      , allowance = TokenValue.tokenValue data.allowedBalance
      , staked = TokenValue.tokenValue data.stakedBalance
      , claimableRewards = TokenValue.tokenValue data.earned
      , rewardRate = TokenValue.tokenValue data.rewardRate
      , timestamp = TimeHelpers.secondsBigIntToPosixWithWarning data.timestamp
      }
    , unpackApy data.apy
    )


getApy : Int -> (Result Http.Error Float -> msg) -> Cmd msg
getApy networkId msgConstructor =
    Eth.call
        (Config.httpProviderUrl networkId)
        (StakingScripts.getData
            (Config.stakingScriptsAddress networkId)
            (Config.stakingContractAddress networkId)
            (Config.stakingPricePairAddress networkId)
            EthHelpers.zeroAddress
        )
        |> Task.map unpackBindingStruct
        |> Task.map Tuple.second
        |> Task.attempt msgConstructor


unpackApy : BigInt -> Float
unpackApy uintVal =
    (BigIntHelpers.toIntWithWarning uintVal |> toFloat)
        / 1000.0



-- getUnstakedBalance : Address -> (Result Http.Error TokenValue -> msg) -> Cmd msg
-- getUnstakedBalance address msgConstructor =
--     Eth.call
--         Config.httpProviderUrl
--         (ERC20.balanceOf
--             Config.stakingLiquidityContractAddress
--             address
--         )
--         |> Task.attempt
--             (Result.map TokenValue.tokenValue >> msgConstructor)
-- getStakedBalance : Address -> (Result Http.Error TokenValue -> msg) -> Cmd msg
-- getStakedBalance address msgConstructor =
--     Eth.call
--         Config.httpProviderUrl
--         (ERC20.balanceOf
--             Config.stakingContractAddress
--             address
--         )
--         |> Task.attempt
--             (Result.map TokenValue.tokenValue >> msgConstructor)
-- getAmountEarned : Address -> (Result Http.Error TokenValue -> msg) -> Cmd msg
-- getAmountEarned address msgConstructor =
--     Eth.call
--         Config.httpProviderUrl
--         (StakingContract.earned
--             Config.stakingContractAddress
--             address
--         )
--         |> Task.attempt
--             (Result.map TokenValue.tokenValue >> msgConstructor)
