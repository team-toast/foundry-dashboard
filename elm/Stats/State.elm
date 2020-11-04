module Stats.State exposing (..)

import Eth.Types exposing (Address)
import Http
import Json.Decode exposing (Decoder)
import Stats.Types exposing (..)
import Common.Msg exposing (..)


init : ( Model, Cmd Msg )
init =
    ( {}
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


runMsgDown : MsgDown -> Model -> UpdateResult
runMsgDown msg prevModel =
    case msg of
        UpdateWallet _ ->
            justModelUpdate prevModel


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
