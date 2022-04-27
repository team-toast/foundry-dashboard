module Chain exposing (chainDecoder, decodeChain, getColor, getConfig, getName, getProviderUrl, txUrl)

import Config
import Dict exposing (Dict)
import Element exposing (Color)
import Eth.Decode
import Eth.Net
import Eth.Types exposing (TxHash)
import Eth.Utils
import Helpers.Eth
import Json.Decode as Decode exposing (Decoder)
import Result.Extra
import Theme
import Types exposing (ChainConfig, ChainConfigs, ChainId, Flags)


getProviderUrl : ChainId -> ChainConfigs -> Maybe String
getProviderUrl chainId chainConfigs =
    chainConfigs
        |> Dict.get chainId
        |> Maybe.map .nodeUrl


getConfig : ChainId -> ChainConfigs -> Maybe ChainConfig
getConfig chainId chainConfigs =
    chainConfigs
        |> Dict.get chainId


txUrl : ChainId -> ChainConfigs -> TxHash -> Maybe String
txUrl chainId chainConfigs hash =
    chainConfigs
        |> Dict.get chainId
        |> Maybe.map .explorerUrl
        |> Maybe.map (\i -> i ++ Eth.Utils.txHashToString hash)


getColor : ChainId -> Dict ChainId Color -> Color
getColor chainId chainColors =
    chainColors
        |> Dict.get chainId
        |> Maybe.withDefault Theme.xDai


getName : ChainId -> ChainConfigs -> Maybe String
getName chainId chainConfigs =
    chainConfigs
        |> Dict.get chainId
        |> Maybe.map .name


chainDecoder : Flags -> Decoder (List Types.ChainConfig)
chainDecoder flags =
    Decode.map
        (\networkId ->
            { name = "Ethereum"
            , networkId = 1
            , nodeUrl = Config.ethereumProviderUrl
            , explorerUrl = "https://etherscan.io/"
            }
        )
        (Decode.field "networkId" decodeChain
            |> Decode.andThen
                (Result.Extra.unpack
                    (always (Decode.fail "bad network"))
                    Decode.succeed
                )
        )
        |> Decode.list


decodeChain : Decoder (Result Types.WalletConnectErr ChainId)
decodeChain =
    Eth.Net.networkIdDecoder
        |> Decode.map
            (\network ->
                Ok (Eth.Net.networkIdToInt network)
            )
