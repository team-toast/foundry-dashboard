module Contracts.BucketSale.Generated.BucketSaleScripts exposing
    ( GetGeneralInfo
    , exitMany
    , getExitInfo
    , getGeneralInfo
    , getGeneralInfoDecoder
    )

import Eth.Abi.Decode as D exposing (abiDecode, andMap, data, toElmDecoder, topic)
import Eth.Abi.Encode as E exposing (Encoding(..), abiEncode)
import BigInt exposing (BigInt)
import Eth.Types exposing (..)
import Eth.Utils as U
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (custom)



{-

   This file was generated by https://github.com/cmditch/elm-ethereum-generator v4.0.0
   Compatible with elm-ethereum v4.0.0

-}
-- exitMany(address,address,uint256[]) function


exitMany : Address -> Address -> Address -> List (BigInt) -> Call ()
exitMany contractAddress bucketSale_ buyer_ bucketIds_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "98baa8ca" [ E.address bucketSale_, E.address buyer_, (E.list << List.map E.uint) bucketIds_ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


-- getExitInfo(address,address) function


getExitInfo : Address -> Address -> Address -> Call (List (BigInt))
getExitInfo contractAddress bucketSale_ buyer_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "06894f8d" [ E.address bucketSale_, E.address buyer_ ]
    , nonce = Nothing
    , decoder = toElmDecoder (D.staticArray 1201 D.uint)
    }


-- getGeneralInfo(address,address,uint256) function


type alias GetGeneralInfo =
    { totalExitedTokens : BigInt
    , bucket_totalValueEntered : BigInt
    , buy_valueEntered : BigInt
    , buy_buyerTokensExited : BigInt
    , tokenSoldForAllowance : BigInt
    , tokenSoldForBalance : BigInt
    , ethBalance : BigInt
    , tokenOnSaleBalance : BigInt
    , exitInfo : List (BigInt)
    }


getGeneralInfo : Address -> Address -> Address -> BigInt -> Call GetGeneralInfo
getGeneralInfo contractAddress bucketSale_ buyer_ bucketId_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "9fe5d76c" [ E.address bucketSale_, E.address buyer_, E.uint bucketId_ ]
    , nonce = Nothing
    , decoder = getGeneralInfoDecoder
    }


getGeneralInfoDecoder : Decoder GetGeneralInfo
getGeneralInfoDecoder =
    abiDecode GetGeneralInfo
        |> andMap D.uint
        |> andMap D.uint
        |> andMap D.uint
        |> andMap D.uint
        |> andMap D.uint
        |> andMap D.uint
        |> andMap D.uint
        |> andMap D.uint
        |> andMap (D.staticArray 1201 D.uint)
        |> toElmDecoder


