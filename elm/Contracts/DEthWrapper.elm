module Contracts.DEthWrapper exposing (..)

import BigInt
import Config exposing (derivedEthContractAddress)
import Contracts.Generated.DEth as Death
import Contracts.Generated.ERC20 as ERC20
import Eth
import Eth.Types as Eth exposing (Address)
import Http
import Task
import TokenValue exposing (TokenValue)
import Types exposing (Chain)


deposit : Address -> TokenValue -> Eth.Send
deposit sender amount =
    Death.squanderMyEthForWorthlessBeans
        derivedEthContractAddress
        sender
        |> Eth.toSend


redeem : Address -> TokenValue -> Eth.Send
redeem receiver amount =
    Death.redeem
        derivedEthContractAddress
        receiver
        (TokenValue.getEvmValue amount)
        |> Eth.toSend


getIssuanceDetail : TokenValue -> (Result Http.Error ( TokenValue, TokenValue, TokenValue ) -> msg) -> Chain -> Cmd msg
getIssuanceDetail amount msgConstructor chain =
    Eth.call
        (Config.httpProviderUrl chain)
        (Death.calculateIssuanceAmount
            derivedEthContractAddress
            (TokenValue.getEvmValue amount)
        )
        |> Task.map unpackCalculatedIssuanceAmount
        |> Task.attempt msgConstructor


getRedeemable : TokenValue -> (Result Http.Error ( TokenValue, TokenValue, TokenValue ) -> msg) -> Chain -> Cmd msg
getRedeemable amount msgConstructor chain =
    Eth.call
        (Config.httpProviderUrl chain)
        (Death.calculateRedemptionValue
            derivedEthContractAddress
            (TokenValue.getEvmValue amount)
        )
        |> Task.map unpackCalculatedRedemptionValue
        |> Task.attempt msgConstructor


unpackCalculatedRedemptionValue : Death.CalculateRedemptionValue -> ( TokenValue, TokenValue, TokenValue )
unpackCalculatedRedemptionValue data =
    ( TokenValue.tokenValue data.collateralRedeemed
    , TokenValue.tokenValue <| BigInt.add data.protocolFee data.automationFee
    , TokenValue.tokenValue data.collateralReturned
    )


approveDEthToken : Eth.Send
approveDEthToken =
    ERC20.approve
        Config.derivedEthContractAddress
        Config.derivedEthContractAddress
        (TokenValue.maxTokenValue |> TokenValue.getEvmValue)
        |> Eth.toSend


unpackCalculatedIssuanceAmount : Death.CalculateIssuanceAmount -> ( TokenValue, TokenValue, TokenValue )
unpackCalculatedIssuanceAmount data =
    ( TokenValue.tokenValue data.actualCollateralAdded
    , TokenValue.tokenValue <| BigInt.add data.protocolFee data.automationFee
    , TokenValue.tokenValue data.tokensIssued
    )
