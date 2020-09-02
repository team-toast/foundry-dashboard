module Sentiment.State exposing (..)

import Http
import Json.Decode exposing (Decoder)
import Sentiment.Types exposing (..)


init : ( Model, Cmd Msg )
init =
    ( { polls = Nothing }
    , fetchAllDataCmd
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
            Debug.todo ""

        TestVote ->
            UpdateResult
                prevModel
                -- sendTestVoteCmd
                Cmd.none
                []

        AllDataFetched responses ->
            let
                _ =
                    Debug.log "responses" responses
            in
            justModelUpdate
                prevModel
        


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


fetchAllPollsCmd : Cmd Msg
fetchAllPollsCmd =
    Http.get
        { url = "https://personal-rxyx.outsystemscloud.com/QuantumObserver/rest/VotingResults/GetPolls"
        , expect =
            Http.expectJson
                PollsFetched
                (Json.Decode.list pollDecoder)
        }



-- sendTestVoteCmd : Cmd Msg
-- sendTestVoteCmd =
--     Http.post
--         { url = "https://personal-rxyx.outsystemscloud.com/QuantumObserver/rest/VotingResults/PlaceVote"
--         , body = Http.jsonBody
--         , expect = Http.expectWhatever VoteSent
--         }


fetchAllDataCmd : Cmd Msg
fetchAllDataCmd =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "FromPollId" "0"
            , Http.header "Count" "2"
            ]
        , url = "https://personal-rxyx.outsystemscloud.com/QuantumObserver/rest/VotingResults/GetPollVotes"
        , body = Http.emptyBody
        , expect = Http.expectJson AllDataFetched allDataDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


pollDecoder : Decoder Poll
pollDecoder =
    Json.Decode.map3
        (\a b c -> Poll a b c [])
        (Json.Decode.field "Id" Json.Decode.int)
        (Json.Decode.field "Title" Json.Decode.string)
        (Json.Decode.field "Question" Json.Decode.string)



-- (Json.Decode.field "Id" Json.Decode.int)


allDataDecoder : Decoder (List Response)
allDataDecoder =
    Json.Decode.list responseDecoder


responseDecoder : Decoder Response
responseDecoder =
    Debug.todo "decode response"
