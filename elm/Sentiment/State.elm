port module Sentiment.State exposing (..)

import Common.Msg exposing (..)
import Common.Types exposing (..)
import Config
import Eth
import Eth.Types exposing (Address)
import Eth.Utils
import Http
import Json.Decode exposing (Decoder)
import Json.Encode
import Sentiment.Types exposing (..)
import Task
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

        OptionClicked userInfo ( questionString, answerString ) ->
            UpdateResult
                prevModel
                (signResponseCmd questionString answerString userInfo)
                []

        Web3SignResultValue jsonVal ->
            let
                _ =
                    Debug.log "signResult" jsonVal
            in
            justModelUpdate prevModel



-- AllDataFetched responses ->
--     let
--         _ =
--             Debug.log "responses" responses
--     in
--     justModelUpdate
--         prevModel


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


signResponseCmd : String -> String -> UserInfo -> Cmd Msg
signResponseCmd questionString responseString userInfo =
    web3Sign <|
        Json.Encode.object
            [ ( "data", Json.Encode.string <| encodeResponse questionString responseString )
            , ( "account", Json.Encode.string (userInfo.address |> Eth.Utils.addressToChecksumString) )
            ]


encodeResponse : String -> String -> String
encodeResponse question answer =
    Json.Encode.object
        [ ( "context", Json.Encode.string "FRY Holder Sentiment Voting" )
        , ( "question", Json.Encode.string question )
        , ( "answer", Json.Encode.string answer )
        ]
        |> Json.Encode.encode 0


subscriptions : Model -> Sub Msg
subscriptions model =
    web3SignResult Web3SignResultValue


port web3Sign : Json.Decode.Value -> Cmd msg


port web3SignResult : (Json.Decode.Value -> msg) -> Sub msg
