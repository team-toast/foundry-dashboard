module Contracts.DEthWrapper exposing (..)

import Config
import Contracts.Generated.DEth as Death
import Eth.Types exposing (Address)
import TokenValue exposing (TokenValue)


getBalance : Address -> TokenValue
getBalance address =
    Death.balanceOf
        Config.derivedEthContractAddress
        address


getRedemptionValue : Address -> TokenValue -> TokenValue
getRedemptionValue address dEthBalance =
    let
        nrTokens =
            case dEthBalance of
                Just val ->
                    val

                Nothing ->
                    0
    in
    Death.calculateRedemptionValue
        address
        nrTokens
