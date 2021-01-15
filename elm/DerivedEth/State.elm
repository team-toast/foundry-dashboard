module DerivedEth.State exposing (..)

import Array
import Common.Msg exposing (..)
import Config
import DerivedEth.Types exposing (..)
import Eth.Types exposing (Address)
import Http
import Json.Decode exposing (Decoder, value)
import Time
import TokenValue


init :
    Int
    -> ( Model, Cmd Msg )
init nowInMillis =
    ( { currentTime = nowInMillis
      }
    , Cmd.none
    )


update :
    Msg
    -> Model
    -> UpdateResult
update msg prevModel =
    case msg of
        MsgUp msgUp ->
            UpdateResult
                prevModel
                Cmd.none
                [ msgUp ]

        Tick i ->
            UpdateResult
                prevModel
                Cmd.none
                []


runMsgDown :
    MsgDown
    -> Model
    -> UpdateResult
runMsgDown msg prevModel =
    case msg of
        UpdateWallet _ ->
            justModelUpdate prevModel


subscriptions :
    Model
    -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 5000 Tick ]
