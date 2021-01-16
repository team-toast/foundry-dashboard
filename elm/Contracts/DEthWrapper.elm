module Contracts.DEthWrapper exposing (..)

import Config
import Contracts.Generated.DEth as Death
import Eth.Types exposing (Address)
import Http
import Task
import TokenValue exposing (TokenValue)


getRedemptionValue :
    Address
    -> TokenValue
    -> TokenValue
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
