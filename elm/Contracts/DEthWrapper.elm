module Contracts.DEthWrapper exposing (..)

import Contracts.Generated.DEth as Death
import Contracts.Generated.ERC20 as ERC20
import Eth
import Eth.Types as Eth exposing (Address)
import Http
import Task exposing (Task)
import TokenValue exposing (TokenValue)


deposit : Address -> Address -> TokenValue -> Eth.Send
deposit contract sender amount =
    Death.squanderMyEthForWorthlessBeans
        contract
        sender
        |> (\call ->
                { call
                    | value =
                        TokenValue.getEvmValue amount
                            |> Just
                }
           )
        |> Eth.toSend


redeem : Address -> Address -> TokenValue -> Eth.Send
redeem contract receiver amount =
    Death.redeem
        contract
        receiver
        (TokenValue.getEvmValue amount)
        |> Eth.toSend


getIssuanceDetail : Address -> String -> TokenValue -> Task Http.Error Death.CalculateIssuanceAmount
getIssuanceDetail contract httpProvider amount =
    Death.calculateIssuanceAmount
        contract
        (TokenValue.getEvmValue amount)
        |> Eth.call httpProvider


getRedeemable : Address -> String -> TokenValue -> Task Http.Error Death.CalculateRedemptionValue
getRedeemable contract httpProvider amount =
    Death.calculateRedemptionValue
        contract
        (TokenValue.getEvmValue amount)
        |> Eth.call httpProvider


approveDEthToken : Address -> Eth.Send
approveDEthToken contract =
    ERC20.approve
        contract
        contract
        (TokenValue.maxTokenValue |> TokenValue.getEvmValue)
        |> Eth.toSend
