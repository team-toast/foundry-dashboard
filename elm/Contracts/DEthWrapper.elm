module Contracts.DEthWrapper exposing (..)

import Contracts.Generated.DEth as Deth
import Contracts.Generated.ERC20 as ERC20
import Eth
import Eth.Types as Eth exposing (Address)
import Http
import Task exposing (Task)
import TokenValue exposing (TokenValue)


deposit : Address -> Address -> TokenValue -> Eth.Send
deposit contract sender amount =
    Deth.squanderMyEthForWorthlessBeansAndAgreeToTerms
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
    Deth.redeem
        contract
        receiver
        (TokenValue.getEvmValue amount)
        |> Eth.toSend


getIssuanceDetail : Address -> String -> TokenValue -> Task Http.Error Deth.CalculateIssuanceAmount
getIssuanceDetail contract httpProvider amount =
    Deth.calculateIssuanceAmount
        contract
        (TokenValue.getEvmValue amount)
        |> Eth.call httpProvider


getRedeemable : Address -> String -> TokenValue -> Task Http.Error Deth.CalculateRedemptionValue
getRedeemable contract httpProvider amount =
    Deth.calculateRedemptionValue
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
