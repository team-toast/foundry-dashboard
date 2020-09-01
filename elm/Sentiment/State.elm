module Sentiment.State exposing (..)

import Sentiment.Types exposing (..)


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
