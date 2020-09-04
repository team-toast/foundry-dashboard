port module Sentiment.State exposing (..)

import Common.Msg exposing (..)
import Common.Types exposing (..)
import Config
import Eth
import Eth.Types exposing (Address)
import Eth.Utils
import Helpers.Eth as EthHelpers
import Http
import Json.Decode exposing (Decoder)
import Json.Encode
import List.Extra
import Sentiment.Types exposing (..)
import Task
import Url.Builder
import UserNotice as UN
import Wallet exposing (Wallet)


init : ( Model, Cmd Msg )
init =
    ( { polls = Nothing
      }
    , fetchAllPollsCmd
    )


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        MsgUp msgUp ->
            UpdateResult
                prevModel
                Cmd.none
                [ msgUp ]

        PollsFetched pollsFetchedResult ->
            case pollsFetchedResult of
                Err httpErr ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        [ AddUserNotice <| UN.httpFetchError "fetch polls" httpErr ]

                Ok polls ->
                    justModelUpdate
                        { prevModel
                            | polls = Just polls
                        }

        OptionClicked userInfo poll pollOptionId ->
            UpdateResult
                prevModel
                (signResponseCmd userInfo poll pollOptionId)
                []

        Web3SignResultValue jsonVal ->
            let
                decodedSignResult =
                    Json.Decode.decodeValue signedResponseFromJSDecoder jsonVal
            in
            case decodedSignResult of
                Ok signResult ->
                    UpdateResult
                        prevModel
                        (sendSignedResponseCmd signResult)
                        []

                Err errStr ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        [ AddUserNotice <| UN.signingError <| Json.Decode.errorToString errStr ]

        ResponseSent pollId sendResult ->
            case sendResult of
                Ok _ ->
                    let
                        _ =
                            Debug.log "sent. refreshing..." ""
                    in
                    UpdateResult
                        prevModel
                        (refreshPollVotesCmd pollId)
                        []

                Err httpErr ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        [ AddUserNotice <| UN.httpSendError "send response" httpErr ]

        SignedResponsesFetched responsesFetchedResult ->
            let
                _ =
                    Debug.log "responsesFetchedResult" responsesFetchedResult
            in
            justModelUpdate prevModel


fetchAllPollsCmd : Cmd Msg
fetchAllPollsCmd =
    Http.request
        { method = "GET"
        , headers = []
        , url = "https://personal-rxyx.outsystemscloud.com/QuantumObserver/rest/VotingResults/GetPolls?FromPollId=0&Count=0"
        , body = Http.emptyBody
        , expect =
            Http.expectJson
                PollsFetched
                pollListDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


pollListDecoder : Decoder (List Poll)
pollListDecoder =
    Json.Decode.list pollDecoder
        |> Json.Decode.map (List.sortBy .id)


pollDecoder : Decoder Poll
pollDecoder =
    Json.Decode.map4 Poll
        (Json.Decode.field "Id" Json.Decode.int)
        (Json.Decode.field "Title" Json.Decode.string)
        (Json.Decode.field "Question" Json.Decode.string)
        (Json.Decode.oneOf
            [ pollOptionListDecoder
            , Json.Decode.succeed []
            ]
        )


pollOptionListDecoder : Decoder (List PollOption)
pollOptionListDecoder =
    Json.Decode.field "Options" (Json.Decode.list pollOptionDecoder)
        |> Json.Decode.map (List.sortBy .id)


pollOptionDecoder : Decoder PollOption
pollOptionDecoder =
    Json.Decode.map3 PollOption
        (Json.Decode.field "Id" Json.Decode.int)
        (Json.Decode.field "PollId" Json.Decode.int)
        (Json.Decode.field "Name" Json.Decode.string)


signResponseCmd : UserInfo -> Poll -> Int -> Cmd Msg
signResponseCmd userInfo poll pollOptionId =
    web3Sign <|
        Json.Encode.object
            [ ( "data"
              , Json.Encode.string <|
                    encodeResponse
                        poll.question
                        (poll.options
                            |> List.filter (.id >> (==) pollOptionId)
                            |> List.head
                            |> Maybe.map .name
                            |> Maybe.withDefault ("[invalid option " ++ String.fromInt pollOptionId ++ "]")
                        )
              )
            , ( "address", Json.Encode.string (userInfo.address |> Eth.Utils.addressToChecksumString) )
            , ( "pollId", Json.Encode.int poll.id )
            , ( "pollOptionId", Json.Encode.int pollOptionId )
            ]


encodeResponse : String -> String -> String
encodeResponse question answer =
    Json.Encode.object
        [ ( "context", Json.Encode.string "FRY Holder Sentiment Voting" )
        , ( "question", Json.Encode.string question )
        , ( "answer", Json.Encode.string answer )
        ]
        |> Json.Encode.encode 0


signedResponseFromJSDecoder : Json.Decode.Decoder SignedResponse
signedResponseFromJSDecoder =
    Json.Decode.map4 SignedResponse
        (Json.Decode.field "address" EthHelpers.addressDecoder)
        (Json.Decode.field "pollId" Json.Decode.int)
        (Json.Decode.field "pollOptionId" Json.Decode.int)
        (Json.Decode.field "sig" Json.Decode.string)


sendSignedResponseCmd : SignedResponse -> Cmd Msg
sendSignedResponseCmd signedResponse =
    let
        url =
            Url.Builder.custom
                (Url.Builder.CrossOrigin "https://personal-rxyx.outsystemscloud.com")
                [ "QuantumObserver", "rest", "VotingResults", "PlaceVote" ]
                []
                Nothing
    in
    Http.post
        { url = url
        , body =
            Http.jsonBody <|
                encodeSignedResponseForServer signedResponse
        , expect = Http.expectWhatever (ResponseSent signedResponse.pollId)
        }


refreshPollVotesCmd : Int -> Cmd Msg
refreshPollVotesCmd pollId =
    let
        url =
            Url.Builder.custom
                (Url.Builder.CrossOrigin "https://personal-rxyx.outsystemscloud.com")
                [ "QuantumObserver", "rest", "VotingResults", "GetPollVotes" ]
                [ Url.Builder.int "FromPollId" pollId
                , Url.Builder.int "Count" 1
                ]
                Nothing
    in
    Http.get
        { url = url
        , expect =
            Http.expectJson
                SignedResponsesFetched
                (Json.Decode.list signedResponseFromServerDecoder)
        }


signedResponseFromServerDecoder : Json.Decode.Decoder SignedResponse
signedResponseFromServerDecoder =
    Json.Decode.field "Vote"
        (Json.Decode.map4 SignedResponse
            (Json.Decode.field "Address" <| EthHelpers.addressDecoder)
            (Json.Decode.field "PollId" <| Json.Decode.int)
            (Json.Decode.field "OptionId" <| Json.Decode.int)
            (Json.Decode.field "Signature" <| Json.Decode.string)
        )


encodeSignedResponseForServer : SignedResponse -> Json.Encode.Value
encodeSignedResponseForServer signedResponse =
    Json.Encode.object
        [ ( "VoteInput"
          , Json.Encode.object
                [ ( "Address", Json.Encode.string (signedResponse.userAddress |> Eth.Utils.addressToChecksumString) )
                , ( "PollId", Json.Encode.int signedResponse.pollId )
                , ( "OptionId", Json.Encode.int signedResponse.pollOptionId )
                , ( "Signature", Json.Encode.string signedResponse.sig )
                , ( "OptionData", Json.Encode.string "" )
                ]
          )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    web3SignResult Web3SignResultValue


port web3Sign : Json.Decode.Value -> Cmd msg


port web3SignResult : (Json.Decode.Value -> msg) -> Sub msg
