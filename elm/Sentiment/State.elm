port module Sentiment.State exposing (..)

import AddressDict exposing (AddressDict)
import Common.Msg exposing (..)
import Common.Types exposing (..)
import Config
import Contracts.FryBalanceFetch
import Dict exposing (Dict)
import Dict.Extra
import Eth
import Eth.Types exposing (Address)
import Eth.Utils
import Helpers.Eth as EthHelpers
import Helpers.Time as TimeHelpers
import Http
import Json.Decode exposing (Decoder)
import Json.Encode
import List.Extra
import Maybe.Extra
import Sentiment.Types exposing (..)
import Set exposing (Set)
import Task
import Time
import TokenValue exposing (TokenValue)
import Url.Builder
import UserNotice as UN
import Wallet exposing (Wallet)


init : ( Model, Cmd Msg )
init =
    ( { polls = Nothing
      , maybeValidResponses = Dict.empty
      , validatedResponses = Dict.empty
      , fryBalances = AddressDict.empty
      , mouseoverState = None
      }
    , Cmd.batch
        [ fetchAllPollsCmd
        ]
    )


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        MsgUp msgUp ->
            UpdateResult
                prevModel
                Cmd.none
                [ msgUp ]

        RefreshAll ->
            UpdateResult
                prevModel
                (Cmd.batch
                    [ refreshPollVotesCmd Nothing
                    , fetchFryBalancesCmd (prevModel.fryBalances |> AddressDict.keys)
                    ]
                )
                []

        PollsFetched pollsFetchedResult ->
            case pollsFetchedResult of
                Err httpErr ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        [ AddUserNotice <| UN.httpFetchError "fetch polls" httpErr ]

                Ok polls ->
                    UpdateResult
                        { prevModel
                            | polls = Just polls
                        }
                        (refreshPollVotesCmd Nothing)
                        []

        OptionClicked userInfo poll maybePollOptionId ->
            case userInfo of
                Nothing ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        []

                Just val ->
                    UpdateResult
                        prevModel
                        (signResponseCmd val poll maybePollOptionId)
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

        Web3ValidateSigResultValue jsonVal ->
            let
                decodedSignResult =
                    Json.Decode.decodeValue validateSigResultDecoder jsonVal
            in
            case decodedSignResult of
                Ok ( responseId, validateResult ) ->
                    let
                        ( newValidatedResponses, maybeRespondingAddress, maybeUserNotice ) =
                            case validateResult of
                                Valid ->
                                    let
                                        maybeSignedResponse =
                                            prevModel.maybeValidResponses
                                                |> Dict.get responseId
                                                |> Maybe.map Tuple.second
                                    in
                                    case maybeSignedResponse of
                                        Just signedResponse ->
                                            ( prevModel.validatedResponses
                                                |> insertValidatedResponse ( responseId, signedResponse )
                                            , Just signedResponse.address
                                            , Nothing
                                            )

                                        Nothing ->
                                            ( prevModel.validatedResponses
                                            , Nothing
                                            , Just <| UN.unexpectedError "got a signature verify result from JS, but for response that I don't have!" responseId
                                            )

                                Invalid ->
                                    ( prevModel.validatedResponses
                                    , Nothing
                                    , Nothing
                                    )

                        msgsUp =
                            maybeUserNotice
                                |> Maybe.map AddUserNotice
                                |> List.singleton
                                |> Maybe.Extra.values

                        newBalancesDict =
                            case maybeRespondingAddress of
                                Nothing ->
                                    prevModel.fryBalances

                                Just address ->
                                    let
                                        newDictPortion =
                                            [ ( address
                                              , Nothing
                                              )
                                            ]
                                                |> AddressDict.fromList
                                    in
                                    AddressDict.union
                                        prevModel.fryBalances
                                        newDictPortion

                        cmd =
                            newBalancesDict
                                |> AddressDict.filter
                                    (\addressString maybeBalance ->
                                        maybeBalance == Nothing
                                    )
                                |> AddressDict.keys
                                |> fetchFryBalancesCmd
                    in
                    UpdateResult
                        { prevModel
                            | validatedResponses = newValidatedResponses
                            , maybeValidResponses =
                                prevModel.maybeValidResponses
                                    |> Dict.update responseId
                                        (Maybe.map
                                            (Tuple.mapFirst
                                                (always True)
                                            )
                                        )
                            , fryBalances = newBalancesDict
                        }
                        cmd
                        msgsUp

                Err errStr ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        [ AddUserNotice <| UN.unexpectedError "error decoding signature validation from web3js" errStr
                        ]

        ResponseSent pollId sendResult ->
            case sendResult of
                Ok _ ->
                    UpdateResult
                        prevModel
                        (refreshPollVotesCmd <| Just pollId)
                        []

                Err httpErr ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        [ AddUserNotice <| UN.httpSendError "send response" httpErr ]

        SignedResponsesFetched responsesFetchedResult ->
            case responsesFetchedResult of
                Ok decodedLoggedSignedResponses ->
                    case prevModel.polls of
                        Nothing ->
                            UpdateResult
                                prevModel
                                Cmd.none
                                [ AddUserNotice <| UN.unexpectedError "Responses were fetched, but the polls haven't loaded yet!" Nothing ]

                        Just polls ->
                            let
                                newMaybeValidResponses =
                                    Dict.union
                                        prevModel.maybeValidResponses
                                        (decodedLoggedSignedResponses
                                            |> Dict.map
                                                (\_ signedResponse ->
                                                    ( False, signedResponse )
                                                )
                                        )

                                responsesToValidate =
                                    newMaybeValidResponses
                                        |> Dict.Extra.filterMap
                                            (\_ ( isValidated, signedResponse ) ->
                                                if not isValidated then
                                                    Just signedResponse

                                                else
                                                    Nothing
                                            )
                                        |> Dict.toList
                                        |> List.map (loggedSignedResponseToResponseToValidate polls)
                                        |> Maybe.Extra.values
                            in
                            UpdateResult
                                { prevModel
                                    | maybeValidResponses = newMaybeValidResponses
                                }
                                (validateSignedResponsesCmd responsesToValidate)
                                []

                Err decodeErr ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        [ AddUserNotice <| UN.unexpectedError "error decoding responses from server" decodeErr ]

        FryBalancesFetched fetchResult ->
            case fetchResult of
                Ok newFryBalances ->
                    justModelUpdate
                        { prevModel
                            | fryBalances =
                                AddressDict.union
                                    (newFryBalances
                                        |> AddressDict.map (always Just)
                                    )
                                    prevModel.fryBalances
                        }

                Err httpErr ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        [ AddUserNotice <| UN.web3FetchError "fetch polls" httpErr ]

        SetMouseoverState newState ->
            justModelUpdate
                { prevModel
                    | mouseoverState = newState
                }


runMsgDown : MsgDown -> Model -> UpdateResult
runMsgDown msg prevModel =
    case msg of
        UpdateWallet _ ->
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


signResponseCmd : UserInfo -> Poll -> Maybe Int -> Cmd Msg
signResponseCmd userInfo poll maybePollOptionId =
    web3Sign <|
        Json.Encode.object
            [ ( "data"
              , Json.Encode.string <|
                    encodeSignableResponse
                        poll
                        maybePollOptionId
              )
            , ( "address", Json.Encode.string (userInfo.address |> Eth.Utils.addressToChecksumString) )
            , ( "pollId", Json.Encode.int poll.id )
            , ( "pollOptionId", encodeIntOrNull maybePollOptionId )
            ]


encodeIntOrNull : Maybe Int -> Json.Encode.Value
encodeIntOrNull =
    Maybe.map Json.Encode.int >> Maybe.withDefault Json.Encode.null


encodeSignedResponse : SignedResponse -> Json.Encode.Value
encodeSignedResponse signedResponse =
    Json.Encode.object
        [ ( "address", EthHelpers.encodeAddress signedResponse.address )
        , ( "pollId", Json.Encode.int signedResponse.pollId )
        , ( "pollOptionId", encodeIntOrNull signedResponse.maybePollOptionId )
        , ( "sig", Json.Encode.string signedResponse.sig )
        ]


encodeResponseToValidate : ResponseToValidate -> Json.Encode.Value
encodeResponseToValidate responseToValidate =
    Json.Encode.object
        [ ( "id", Json.Encode.int responseToValidate.id )
        , ( "data", Json.Encode.string responseToValidate.data )
        , ( "sig", Json.Encode.string responseToValidate.sig )
        , ( "address", EthHelpers.encodeAddress responseToValidate.address )
        ]


signedResponseFromJSDecoder : Json.Decode.Decoder SignedResponse
signedResponseFromJSDecoder =
    Json.Decode.map4 SignedResponse
        (Json.Decode.field "address" EthHelpers.addressDecoder)
        (Json.Decode.field "pollId" Json.Decode.int)
        (Json.Decode.field "pollOptionId" (Json.Decode.nullable Json.Decode.int))
        (Json.Decode.field "sig" Json.Decode.string)


validateSigResultDecoder : Json.Decode.Decoder ( Int, SigValidationResult )
validateSigResultDecoder =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "success" Json.Decode.bool
            |> Json.Decode.map
                (\successBool ->
                    if successBool then
                        Valid

                    else
                        Invalid
                )
        )


fetchFryBalancesCmd : List Address -> Cmd Msg
fetchFryBalancesCmd addresses =
    Contracts.FryBalanceFetch.fetch
        addresses
        FryBalancesFetched


validateSignedResponsesCmd : List ResponseToValidate -> Cmd Msg
validateSignedResponsesCmd loggedSignedResponses =
    loggedSignedResponses
        |> List.map encodeResponseToValidate
        |> List.map web3ValidateSig
        |> Cmd.batch


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


refreshPollVotesCmd : Maybe Int -> Cmd Msg
refreshPollVotesCmd maybePollId =
    let
        url =
            Url.Builder.custom
                (Url.Builder.CrossOrigin "https://personal-rxyx.outsystemscloud.com")
                [ "QuantumObserver", "rest", "VotingResults", "GetPollVotes" ]
                (case maybePollId of
                    Just pollId ->
                        [ Url.Builder.int "FromPollId" pollId
                        , Url.Builder.int "Count" 1
                        ]

                    Nothing ->
                        [ Url.Builder.int "FromPollId" 0
                        , Url.Builder.int "Count" 0
                        ]
                )
                Nothing
    in
    Http.get
        { url = url
        , expect =
            Http.expectJson
                SignedResponsesFetched
                signedResponsesDictFromServerDecoder
        }


signedResponsesDictFromServerDecoder : Json.Decode.Decoder (Dict Int SignedResponse)
signedResponsesDictFromServerDecoder =
    Json.Decode.list Json.Decode.value
        |> Json.Decode.map (List.map (Json.Decode.decodeValue loggedSignedResponseFromServerDecoder))
        |> Json.Decode.map (List.filterMap Result.toMaybe)
        |> Json.Decode.map Dict.fromList


loggedSignedResponseFromServerDecoder : Json.Decode.Decoder ( Int, SignedResponse )
loggedSignedResponseFromServerDecoder =
    Json.Decode.field "Vote"
        (Json.Decode.map2 Tuple.pair
            (Json.Decode.field "Id" Json.Decode.int)
            (Json.Decode.map4 SignedResponse
                (Json.Decode.field "Address" <| EthHelpers.addressDecoder)
                (Json.Decode.field "PollId" <| Json.Decode.int)
                (Json.Decode.maybe <| Json.Decode.field "OptionId" Json.Decode.int)
                (Json.Decode.field "Signature" <| Json.Decode.string)
            )
        )


encodeSignedResponseForServer : SignedResponse -> Json.Encode.Value
encodeSignedResponseForServer signedResponse =
    Json.Encode.object
        [ ( "Address", EthHelpers.encodeAddress signedResponse.address )
        , ( "PollId", Json.Encode.int signedResponse.pollId )
        , ( "OptionId", encodeIntOrNull signedResponse.maybePollOptionId )
        , ( "Signature", Json.Encode.string signedResponse.sig )
        , ( "OptionData", Json.Encode.string "" )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 4000 <| always RefreshAll
        , web3SignResult Web3SignResultValue
        , web3ValidateSigResult Web3ValidateSigResultValue
        ]


port web3Sign : Json.Decode.Value -> Cmd msg


port web3SignResult : (Json.Decode.Value -> msg) -> Sub msg


port web3ValidateSig : Json.Decode.Value -> Cmd msg


port web3ValidateSigResult : (Json.Decode.Value -> msg) -> Sub msg
