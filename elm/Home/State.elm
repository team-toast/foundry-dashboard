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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
