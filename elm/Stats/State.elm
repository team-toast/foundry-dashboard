module Stats.State exposing (..)

import Http
import Json.Decode exposing (Decoder)
import Stats.Types exposing (..)
import Eth.Types exposing (Address)
import Config



init : ( Model, Cmd Msg )
init =
    ( { addressMultiSig = Config.teamToastMultiSigAddress
      , addressTreasury = Config.treasuryForwarderAddress
      , addressFryToken = Config.fryTokenAddress
      , addressBucketSale = Config.bucketSaleAddress
      }
    , Cmd.none
    )


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        MsgUp msgUp ->
            UpdateResult
                prevModel
                Cmd.none
                [ msgUp ]
        


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

