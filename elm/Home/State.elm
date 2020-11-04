module Home.State exposing (..)

import Common.Msg exposing (..)
import Home.Types exposing (..)
import Routing


init : UpdateResult
init =
    UpdateResult
        {}
        Cmd.none
        [ GotoRoute Routing.Sentiment ]


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        MsgUp msgUp ->
            UpdateResult
                prevModel
                Cmd.none
                [ msgUp ]


runMsgDown : MsgDown -> Model -> UpdateResult
runMsgDown _ prevModel =
    justModelUpdate prevModel


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
