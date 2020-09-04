module Sentiment.Types exposing (..)

import Common.Msg exposing (..)
import Common.Types exposing (..)
import Dict exposing (Dict)
import Eth.Types exposing (Address)
import Eth.Utils
import Http
import Json.Decode
import Time


type alias Model =
    { polls : Maybe (List Poll)
    , validatedResponses : ValidatedResponseTracker
    }


type Msg
    = MsgUp MsgUp
    | PollsFetched (Result Http.Error (List Poll))
    | OptionClicked UserInfo Poll Int
    | Web3SignResultValue Json.Decode.Value
    | ResponseSent Int (Result Http.Error ())
    | SignedResponsesFetched (Result Http.Error (List LoggedSignedResponse))


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


type alias ValidatedResponseTracker =
    Dict Int (Dict String ValidatedResponse)


getValidatedResponse : Int -> Address -> ValidatedResponseTracker -> Maybe ValidatedResponse
getValidatedResponse pollId address validatedResponseTracker =
    validatedResponseTracker
        |> Dict.get pollId
        |> Maybe.andThen (Dict.get (Eth.Utils.addressToChecksumString address))



-- insertValidatedResponse : Int -> Address -> ValidatedResponse -> ValidatedResponseTracker -> ValidatedResponseTracker
-- insertValidatedResponse pollId address validatedResponse validatedResponseTracker =
--     validatedResponseTracker
--         |> Dict.update pollId
--             (\maybeDict ->
--                 Just
--                     (maybeDict
--                         |> Maybe.withDefault Dict.empty
--                         |> Dict.insert
--                             (Eth.Utils.addressToChecksumString address)
--                             validatedResponse
--                     )
--             )


insertValidatedResponse : LoggedSignedResponse -> ValidatedResponseTracker -> ValidatedResponseTracker
insertValidatedResponse loggedSignedResponse validatedResponseTracker =
    let
        validatedResponse =
            { id = loggedSignedResponse.id
            , pollOptionId = loggedSignedResponse.signedResponse.pollOptionId
            }
    in
    validatedResponseTracker
        |> Dict.update loggedSignedResponse.signedResponse.pollId
            (\maybeDict ->
                Just
                    (maybeDict
                        |> Maybe.withDefault Dict.empty
                        |> Dict.insert
                            (Eth.Utils.addressToChecksumString loggedSignedResponse.signedResponse.address)
                            validatedResponse
                    )
            )


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
    { address : Address
    , pollId : Int
    , pollOptionId : Int
    , sig : String
    }


type alias LoggedSignedResponse =
    { id : Int
    , signedResponse : SignedResponse
    }


type alias ValidatedResponse =
    { id : Int
    , pollOptionId : Int
    }
