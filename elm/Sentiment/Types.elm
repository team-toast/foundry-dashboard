module Sentiment.Types exposing (..)

import Common.Msg exposing (..)
import Common.Types exposing (..)
import Eth.Types exposing (Address)
import Http
import Json.Decode


type alias Model =
    { polls : Maybe (List Poll)
    }


type Msg
    = MsgUp MsgUp
    | PollsFetched (Result Http.Error (List Poll))
    | OptionClicked UserInfo Poll Int
    | Web3SignResultValue Json.Decode.Value
    | ResponseSent Int (Result Http.Error ())
    | SignedResponsesFetched (Result Http.Error (List SignedResponse))


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


type alias SignedResponse =
    { userAddress : Address
    , pollId : Int
    , pollOptionId : Int
    , sig : String
    }



-- type alias RespondingInfo =
--     { pollId : Int
--     , pollOptionId : Int
--     , state : RespondingState
--     }
-- type RespondingState
--     = Signing
--     | Sending
