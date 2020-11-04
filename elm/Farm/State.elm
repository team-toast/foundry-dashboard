module Farm.State exposing (..)

import Common.Msg exposing (MsgDown, MsgUp)
import Farm.Types exposing (..)


init : ( Model, Cmd Msg )
init =
    ( { userBalanceInfo = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        NoOp ->
            justModelUpdate prevModel

        MsgUp msgUp ->
            UpdateResult
                prevModel
                Cmd.none
                [ msgUp ]


runMsgDown : MsgDown -> Model -> UpdateResult
runMsgDown msg prevModel =
    case msg of
        Common.Msg.UpdateWallet _ ->
            justModelUpdate prevModel


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
