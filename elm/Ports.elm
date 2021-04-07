port module Ports exposing (..)

import Json.Decode exposing (Value)



-- OUT


port beginLocationCheck : () -> Cmd msg


port bscImport : () -> Cmd msg


port connectToWeb3 : () -> Cmd msg


port consentToCookies : () -> Cmd msg


port log : String -> Cmd msg


port refreshWallet : String -> Cmd msg


port txOut : Json.Decode.Value -> Cmd msg


port txSend : Value -> Cmd msg


port web3Sign : Json.Decode.Value -> Cmd msg


port web3ValidateSig : Json.Decode.Value -> Cmd msg



-- IN


port chainSwitchResponse : (Value -> msg) -> Sub msg


port locationCheckResult : (Json.Decode.Value -> msg) -> Sub msg


port txIn : (Json.Decode.Value -> msg) -> Sub msg


port txSendResponse : (Value -> msg) -> Sub msg


port walletResponse : (Value -> msg) -> Sub msg


port walletSentryPort : (Json.Decode.Value -> msg) -> Sub msg


port web3SignResult : (Json.Decode.Value -> msg) -> Sub msg


port web3ValidateSigResult : (Json.Decode.Value -> msg) -> Sub msg
