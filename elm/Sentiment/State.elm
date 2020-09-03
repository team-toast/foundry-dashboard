module Sentiment.State exposing (..)

import Common.Msg exposing (..)
import Http
import Json.Decode exposing (Decoder)
import Sentiment.Types exposing (..)
import UserNotice as UN


init : ( Model, Cmd Msg )
init =
    ( { polls = Nothing }
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

        OptionClicked pollId optionId ->
            UpdateResult
                prevModel
                -- sendTestVoteCmd
                Cmd.none
                []



-- AllDataFetched responses ->
--     let
--         _ =
--             Debug.log "responses" responses
--     in
--     justModelUpdate
--         prevModel


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


fetchAllPollsCmd : Cmd Msg
fetchAllPollsCmd =
    Http.request
        { method = "GET"
        , headers = []
        , url = "https://personal-rxyx.outsystemscloud.com/QuantumObserver/rest/VotingResults/GetPolls?FromPollId=0&Count=10"
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



-- sendTestVoteCmd : Cmd Msg
-- sendTestVoteCmd =
--     Http.post
--         { url = "https://personal-rxyx.outsystemscloud.com/QuantumObserver/rest/VotingResults/PlaceVote"
--         , body = Http.jsonBody
--         , expect = Http.expectWhatever VoteSent
--         }
-- fetchAllDataCmd : Cmd Msg
-- fetchAllDataCmd =
--     Http.request
--         { method = "GET"
--         , headers =
--             [ Http.header "FromPollId" "0"
--             , Http.header "Count" "2"
--             ]
--         , url = "https://personal-rxyx.outsystemscloud.com/QuantumObserver/rest/VotingResults/GetPollVotes"
--         , body = Http.emptyBody
--         , expect = Http.expectJson AllDataFetched allDataDecoder
--         , timeout = Nothing
--         , tracker = Nothing
--         }
-- (Json.Decode.field "Id" Json.Decode.int)
-- allDataDecoder : Decoder (List Response)
-- allDataDecoder =
--     Json.Decode.list responseDecoder
-- responseDecoder : Decoder Response
-- responseDecoder =
--     Debug.todo "decode response"
