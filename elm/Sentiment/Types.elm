module Sentiment.Types exposing (..)

import Common.Msg exposing (..)
import Http


type alias Model =
    { polls : Maybe (List Poll)
    }


type Msg
    = MsgUp MsgUp
    -- | AllDataFetched (Result Http.Error (List Response))
    | TestVote
    | PollsFetched (Result Http.Error (List Poll))


type alias UpdateResult =
    { newModel : Model
    , cmd : Cmd Msg
    , msgUps : List MsgUp
    }


justModelUpdate : Model -> UpdateResult
justModelUpdate model =
    { newModel = model
    , cmd = Cmd.none
    , msgUps = []
    }


type alias Poll =
    { id : Int
    , title : String
    , question : String
    , options : List PollOption
    }


type alias PollOption =
    { id : Int
    , pollId : Int
    , name : String
    }