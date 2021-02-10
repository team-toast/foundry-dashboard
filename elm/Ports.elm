port module Ports exposing (..)

import Json.Decode


port walletSentryPort : (Json.Decode.Value -> msg) -> Sub msg


port connectToWeb3 : () -> Cmd msg


port txOut : Json.Decode.Value -> Cmd msg


port txIn : (Json.Decode.Value -> msg) -> Sub msg


port gTagOut : Json.Decode.Value -> Cmd msg


port consentToCookies : () -> Cmd msg


port beginLocationCheck : () -> Cmd msg


port locationCheckResult : (Json.Decode.Value -> msg) -> Sub msg


port web3Sign : Json.Decode.Value -> Cmd msg


port web3SignResult : (Json.Decode.Value -> msg) -> Sub msg


port web3ValidateSig : Json.Decode.Value -> Cmd msg


port web3ValidateSigResult : (Json.Decode.Value -> msg) -> Sub msg
