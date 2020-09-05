module Sentiment.Types exposing (..)

import AddressDict exposing (AddressDict)
import Common.Msg exposing (..)
import Common.Types exposing (..)
import Dict exposing (Dict)
import Eth.Types exposing (Address)
import Eth.Utils
import Http
import Json.Decode
import Time
import TokenValue exposing (TokenValue)


type alias Model =
    { polls : Maybe (List Poll)
    , validatedResponses : ValidatedResponseTracker
    , fryBalances : AddressDict (Maybe TokenValue)
    }


type Msg
    = MsgUp MsgUp
    | PollsFetched (Result Http.Error (List Poll))
    | OptionClicked UserInfo Poll Int
    | Web3SignResultValue Json.Decode.Value
    | ResponseSent Int (Result Http.Error ())
    | SignedResponsesFetched (Result Http.Error (List LoggedSignedResponse))
    | FryBalancesFetched (Result Http.Error (AddressDict TokenValue))


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
    Dict Int (AddressDict ValidatedResponse)


getValidatedResponse : Int -> Address -> ValidatedResponseTracker -> Maybe ValidatedResponse
getValidatedResponse pollId address validatedResponseTracker =
    validatedResponseTracker
        |> Dict.get pollId
        |> Maybe.andThen (AddressDict.get address)


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
                        |> Maybe.withDefault AddressDict.empty
                        |> AddressDict.insert
                            loggedSignedResponse.signedResponse.address
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
