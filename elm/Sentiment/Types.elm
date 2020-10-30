module Sentiment.Types exposing (..)

import AddressDict exposing (AddressDict)
import Common.Msg exposing (..)
import Common.Types exposing (..)
import Dict exposing (Dict)
import Eth.Types exposing (Address)
import Eth.Utils
import Http
import Json.Decode
import Json.Encode
import Time
import TokenValue exposing (TokenValue)


type alias Model =
    { polls : Maybe (List Poll)
    , maybeValidResponses : Dict Int ( Bool, SignedResponse ) -- bool represents whether the validation test has been ATTEMPTED, not whether it PASSED
    , validatedResponses : ValidatedResponseTracker
    , fryBalances : AddressDict (Maybe TokenValue)
    , mouseoverState : MouseoverState
    }


type Msg
    = MsgUp MsgUp
    | RefreshAll
    | PollsFetched (Result Http.Error (List Poll))
    | OptionClicked (Maybe UserInfo) Poll (Maybe Int)
    | Web3SignResultValue Json.Decode.Value
    | Web3ValidateSigResultValue Json.Decode.Value
    | ResponseSent Int (Result Http.Error ())
    | SignedResponsesFetched (Result Http.Error (Dict Int SignedResponse))
    | FryBalancesFetched (Result Http.Error (AddressDict TokenValue))
    | SetMouseoverState MouseoverState


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


type MouseoverState
    = None
    | VoterBlock VoterBlockMouseoverInfo


type alias VoterBlockMouseoverInfo =
    { pollId : Int
    , pollOptionId : Int
    , blockId : Int
    }


type alias ValidatedResponseTracker =
    Dict Int (AddressDict ValidatedResponse)


getValidatedResponse : Int -> Address -> ValidatedResponseTracker -> Maybe ValidatedResponse
getValidatedResponse pollId address validatedResponseTracker =
    validatedResponseTracker
        |> Dict.get pollId
        |> Maybe.andThen (AddressDict.get address)


insertValidatedResponse : LoggedSignedResponse -> ValidatedResponseTracker -> ValidatedResponseTracker
insertValidatedResponse ( responseId, signedResponse ) validatedResponseTracker =
    let
        validatedResponse =
            { id = responseId
            , maybePollOptionId = signedResponse.maybePollOptionId
            }
    in
    validatedResponseTracker
        |> Dict.update signedResponse.pollId
            (\maybeDict ->
                Just
                    (maybeDict
                        |> Maybe.withDefault AddressDict.empty
                        |> AddressDict.insert
                            signedResponse.address
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
    , maybePollOptionId : Maybe Int
    , sig : String
    }


type alias ResponseToValidate =
    { id : Int
    , data : String
    , sig : String
    , address : Address
    }


encodeSignableResponse : Poll -> Maybe Int -> String
encodeSignableResponse poll maybePollOptionId =
    let
        questionStr =
            poll.question

        answerStr =
            case maybePollOptionId of
                Just pollOptionId ->
                    poll.options
                        |> List.filter (.id >> (==) pollOptionId)
                        |> List.head
                        |> Maybe.map .name
                        |> Maybe.withDefault ("[invalid option " ++ String.fromInt pollOptionId ++ "]")

                Nothing ->
                    "[none]"
    in
    Json.Encode.object
        [ ( "context", Json.Encode.string "FRY Holder Sentiment Voting" )
        , ( "question", Json.Encode.string questionStr )
        , ( "answer", Json.Encode.string answerStr )
        ]
        |> Json.Encode.encode 0


loggedSignedResponseToResponseToValidate : List Poll -> LoggedSignedResponse -> Maybe ResponseToValidate
loggedSignedResponseToResponseToValidate polls ( responseId, signedResponse ) =
    let
        maybePoll =
            polls
                |> List.filter (.id >> (==) signedResponse.pollId)
                |> List.head
    in
    maybePoll
        |> Maybe.map
            (\poll ->
                { id = responseId
                , data =
                    encodeSignableResponse
                        poll
                        signedResponse.maybePollOptionId
                , sig = signedResponse.sig
                , address = signedResponse.address
                }
            )


type alias LoggedSignedResponse =
    ( Int, SignedResponse )


type alias ValidatedResponse =
    { id : Int
    , maybePollOptionId : Maybe Int
    }


type SigValidationResult
    = Valid
    | Invalid
