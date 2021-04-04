module Misc exposing (..)

import AddressDict
import Array exposing (Array, fromList)
import Browser.Navigation
import Config as ConfigFile
import Contracts.BucketSale.Wrappers as BucketSaleWrappers exposing (getTotalValueEnteredForBucket)
import Contracts.DEthWrapper as Death
import Contracts.ERC20Wrapper as ERC20
import Contracts.FryBalanceFetch
import Contracts.Staking as StakingContract
import Contracts.UniSwapGraph.Object exposing (..)
import Contracts.UniSwapGraph.Object.Bundle as Bundle
import Contracts.UniSwapGraph.Object.Token as Token
import Contracts.UniSwapGraph.Query as Query
import Contracts.UniSwapGraph.Scalar exposing (Id(..))
import Contracts.UniSwapGraph.ScalarCodecs exposing (..)
import Dict exposing (Dict)
import ElementHelpers as EH
import Eth.Net
import Eth.Sentry.Event
import Eth.Sentry.Tx as TxSentry
import Eth.Types exposing (Address)
import Eth.Utils
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Helpers.Eth as EthHelpers
import Helpers.Time as TimeHelpers exposing (toConciseIntervalString)
import Helpers.Tuple exposing (tuple3MapSecond)
import Http
import Json.Decode as Decoder exposing (Decoder, Error, Value)
import Json.Encode
import Maybe.Extra exposing (isNothing)
import Ports exposing (txIn, txOut)
import Result.Extra
import Routing
import Set
import Time
import TokenValue exposing (TokenValue)
import Types exposing (Chain(..), Config, Jurisdiction, JurisdictionCheckStatus, LocationInfo, LoggedSignedResponse, Model, Msg, Poll, PollOption, PriceValue, ResponseToValidate, SigValidationResult, SignedResponse, UserDerivedEthInfo, UserInfo, UserStakingInfo, ValidatedResponse, ValidatedResponseTracker, Wallet)
import Url.Builder
import UserNotice exposing (noWeb3Provider)
import UserTx exposing (TxInfo)


emptyModel : Browser.Navigation.Key -> Time.Posix -> String -> Bool -> Model
emptyModel key now basePath cookieConsent =
    let
        txSentry =
            TxSentry.init
                ( txOut, txIn )
                Types.TxSentryMsg
                (ConfigFile.httpProviderUrl Eth)

        ( wallet, walletNotices ) =
            ( Types.NoneDetected
            , [ noWeb3Provider ]
            )
    in
    { navKey = key
    , basePath = basePath
    , route = Routing.Sentiment
    , wallet = wallet
    , now = now
    , dProfile = EH.Desktop
    , sentries =
        { xDai =
            Eth.Sentry.Event.init (always Types.NoOp) "" |> Tuple.first
        , ethereum =
            Eth.Sentry.Event.init (always Types.NoOp) "" |> Tuple.first
        , bsc =
            Eth.Sentry.Event.init (always Types.NoOp) "" |> Tuple.first
        }
    , txSentry = txSentry
    , showAddressId = Nothing
    , userNotices = walletNotices
    , trackedTxs = []
    , trackedTxsExpanded = False
    , nonRepeatingGTagsSent = []
    , cookieConsentGranted = cookieConsent
    , currentTime = Time.posixToMillis now
    , currentBucketId = Nothing
    , currentBucketTotalEntered = Nothing
    , currentEthPriceUsd = Nothing
    , currentDaiPriceEth = Nothing
    , currentFryPriceEth = Nothing
    , circSupply = Nothing
    , marketCap = Nothing
    , fullyDiluted = Nothing
    , permaFrostedTokens = Nothing
    , teamTokenBalances = Array.initialize 3 (always Nothing)
    , balancerFryBalance = Nothing
    , permaFrostTotalSupply = Nothing
    , permaFrostBalanceLocked = Nothing
    , treasuryBalance = Nothing
    , userDerivedEthInfo = Nothing
    , jurisdictionCheckStatus = Types.WaitingForClick
    , depositAmount = ""
    , withDrawalAmount = ""
    , polls = Nothing
    , maybeValidResponses = Dict.empty -- bool represents whether the validation test has been ATTEMPTED, not whether it PASSED
    , validatedResponses = Dict.empty
    , fryBalances = AddressDict.empty
    , mouseoverState = Types.None
    , userStakingInfo = Nothing
    , apy = Nothing
    , depositWithdrawUXModel = Nothing
    , farmingIsActive = True -- Set to true to enable farming interface - False will show inactive message
    , config = emptyConfig
    }


loadingText : String
loadingText =
    "Loading..."


resultBundle : SelectionSet PriceValue Contracts.UniSwapGraph.Object.Bundle
resultBundle =
    SelectionSet.map PriceValue
        (Bundle.ethPrice
            |> SelectionSet.map
                (\(Contracts.UniSwapGraph.Scalar.BigDecimal dec) ->
                    String.toFloat dec
                        |> Maybe.withDefault 0
                )
        )


resultToken : SelectionSet PriceValue Contracts.UniSwapGraph.Object.Token
resultToken =
    SelectionSet.map PriceValue
        (Token.derivedETH
            |> SelectionSet.map
                (\(Contracts.UniSwapGraph.Scalar.BigDecimal dec) ->
                    String.toFloat dec
                        |> Maybe.withDefault 0
                )
        )


fetchEthPrice : Cmd Msg
fetchEthPrice =
    Query.bundle identity { id = Id "1" } resultBundle
        |> Graphql.Http.queryRequest ConfigFile.uniswapGraphQL
        |> Graphql.Http.send Types.FetchedEthPrice


fetchDaiPrice : Chain -> Cmd Msg
fetchDaiPrice chain =
    Query.token identity
        { id =
            Id <|
                Eth.Utils.addressToString <|
                    ConfigFile.daiContractAddress chain
        }
        resultToken
        |> Graphql.Http.queryRequest ConfigFile.uniswapGraphQL
        |> Graphql.Http.send Types.FetchedDaiPrice


fetchFryPrice : Chain -> Cmd Msg
fetchFryPrice chain =
    Query.token identity
        { id =
            Id <|
                Eth.Utils.addressToString <|
                    ConfigFile.fryContractAddress chain
        }
        resultToken
        |> Graphql.Http.queryRequest ConfigFile.uniswapGraphQL
        |> Graphql.Http.send Types.FetchedFryPrice


fetchTeamTokenBalance : Chain -> Address -> Address -> Int -> Cmd Msg
fetchTeamTokenBalance chain tokenAddress owner index =
    ERC20.getBalanceCmd
        chain
        tokenAddress
        owner
        (Types.FetchedTeamTokens index)


fetchPermaFrostLockedTokenBalance : Chain -> Cmd Msg
fetchPermaFrostLockedTokenBalance chain =
    ERC20.getBalanceCmd
        chain
        ConfigFile.balancerPermafrostPool
        ConfigFile.burnAddress
        Types.FetchedPermaFrostBalanceLocked


fetchPermaFrostTotalSupply : Chain -> Cmd Msg
fetchPermaFrostTotalSupply chain =
    ERC20.getTotalSupply
        chain
        ConfigFile.balancerPermafrostPool
        Types.FetchedPermaFrostTotalSupply


fetchBalancerPoolFryBalance : Chain -> Cmd Msg
fetchBalancerPoolFryBalance chain =
    ERC20.getBalanceCmd
        chain
        (ConfigFile.fryContractAddress chain)
        ConfigFile.balancerPermafrostPool
        Types.FetchedBalancerFryBalance


fetchTreasuryBalance : Chain -> Cmd Msg
fetchTreasuryBalance chain =
    ERC20.getBalanceCmd
        chain
        (ConfigFile.daiContractAddress chain)
        ConfigFile.treasuryForwarderAddress
        Types.FetchedTreasuryBalance


calcCircSupply : Maybe Int -> Array (Maybe TokenValue) -> Maybe TokenValue -> Maybe Float
calcCircSupply currentBucketId totalTeamTokens totalPermaFrostedTokens =
    let
        teamTokens =
            totalTeamTokens |> Array.toList
    in
    case currentBucketId of
        Just bucketId ->
            if List.any isNothing teamTokens then
                Nothing

            else
                let
                    ttt =
                        teamTokens
                            |> List.map (Maybe.withDefault TokenValue.zero)
                            |> List.map TokenValue.toFloatWithWarning
                            |> List.sum
                in
                case totalPermaFrostedTokens of
                    Just tpt ->
                        Just <|
                            toFloat
                                (ConfigFile.bucketSaleTokensPerBucket
                                    * bucketId
                                )
                                + ttt
                                - TokenValue.toFloatWithWarning tpt

                    _ ->
                        Nothing

        _ ->
            Nothing


calcMarketCap :
    Maybe Float
    -> Maybe Float
    -> Maybe Float
    -> Maybe Float
calcMarketCap currentFryPriceEth currentEthPriceUsd circSupply =
    case circSupply of
        Just cs ->
            case currentFryPriceEth of
                Just fry ->
                    case currentEthPriceUsd of
                        Just eth ->
                            cs
                                * fry
                                * eth
                                |> Just

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


calcFullyDilutedMarketCap :
    Maybe Float
    -> Maybe Float
    -> Maybe Float
calcFullyDilutedMarketCap currentFryPriceEth currentEthPriceUsd =
    case currentFryPriceEth of
        Just fry ->
            case currentEthPriceUsd of
                Just eth ->
                    Just <|
                        toFloat ConfigFile.fryTotalSupply
                            * fry
                            * eth

                _ ->
                    Nothing

        _ ->
            Nothing


getCurrentBucketId :
    Int
    -> Maybe Int
getCurrentBucketId now =
    Just <|
        (TimeHelpers.sub (Time.millisToPosix now) (Time.millisToPosix ConfigFile.saleStarted)
            |> TimeHelpers.posixToSeconds
        )
            // (ConfigFile.bucketSaleBucketInterval
                    |> TimeHelpers.posixToSeconds
               )


getBucketRemainingTimeText :
    Maybe Int
    -> Int
    -> String
getBucketRemainingTimeText bucketId now =
    case bucketId of
        Just id ->
            toConciseIntervalString
                (TimeHelpers.sub
                    (getBucketEndTime id)
                    (Time.millisToPosix now)
                )

        _ ->
            loadingText


getBucketStartTime :
    Int
    -> Time.Posix
getBucketStartTime bucketId =
    Time.millisToPosix
        (ConfigFile.saleStarted + (bucketId * Time.posixToMillis ConfigFile.bucketSaleBucketInterval))


getBucketEndTime : Int -> Time.Posix
getBucketEndTime bucketId =
    TimeHelpers.add
        (getBucketStartTime bucketId)
        ConfigFile.bucketSaleBucketInterval


calcEffectivePricePerToken : Maybe TokenValue -> Maybe Float -> Maybe Float -> String
calcEffectivePricePerToken totalValueEntered tokenValueEth ethValueUsd =
    let
        tpb =
            toFloat <|
                if ConfigFile.bucketSaleTokensPerBucket < 1 then
                    1

                else
                    ConfigFile.bucketSaleTokensPerBucket
    in
    case totalValueEntered of
        Just tve ->
            case maybeFloatMultiply tokenValueEth ethValueUsd of
                Just val ->
                    TokenValue.divFloatWithWarning (TokenValue.mulFloatWithWarning tve val) tpb
                        |> TokenValue.toConciseString

                _ ->
                    loadingText

        _ ->
            loadingText


maybeFloatMultiply : Maybe Float -> Maybe Float -> Maybe Float
maybeFloatMultiply val1 val2 =
    case val1 of
        Just v1 ->
            case val2 of
                Just v2 ->
                    Just <| v1 * v2

                _ ->
                    Nothing

        _ ->
            Nothing


fetchTotalValueEnteredCmd : Chain -> Maybe Int -> Cmd Msg
fetchTotalValueEnteredCmd chain bucketId =
    case bucketId of
        Just id ->
            getTotalValueEnteredForBucket
                chain
                id
                (Just id
                    |> Types.BucketValueEnteredFetched
                )

        _ ->
            Cmd.none


calcPermaFrostedTokens : Maybe TokenValue -> Maybe TokenValue -> Maybe TokenValue -> Maybe TokenValue
calcPermaFrostedTokens balancerFryBalance permaFrostBalanceLocked permaFrostTotalSupply =
    case balancerFryBalance of
        Just fryBalance ->
            case permaFrostBalanceLocked of
                Just pfLocked ->
                    case permaFrostTotalSupply of
                        Just pfTotalSupply ->
                            let
                                fry =
                                    TokenValue.toFloatWithWarning fryBalance

                                totalSupply =
                                    TokenValue.toFloatWithWarning pfTotalSupply

                                locked =
                                    TokenValue.toFloatWithWarning pfLocked
                            in
                            locked
                                / totalSupply
                                * fry
                                |> TokenValue.fromFloatWithWarning
                                |> Just

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


calcPermafrostedTokensValue : Maybe TokenValue -> Maybe Float -> Maybe Float -> Maybe TokenValue
calcPermafrostedTokensValue nrTokens tokenEthPrice ethUsdPrice =
    case nrTokens of
        Just tokens ->
            case tokenEthPrice of
                Just ethPrice ->
                    case ethUsdPrice of
                        Just usdPrice ->
                            ((tokens
                                |> TokenValue.toFloatWithWarning
                             )
                                * ethPrice
                                * usdPrice
                                * 2
                            )
                                |> TokenValue.fromFloatWithWarning
                                |> Just

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


calcTreasuryBalance : Maybe Float -> Maybe Float -> Maybe TokenValue -> Maybe TokenValue
calcTreasuryBalance daiPriceInEth ethPriceInUsd numberOfDaiTokens =
    case daiPriceInEth of
        Just daiInEth ->
            case ethPriceInUsd of
                Just ethInUsd ->
                    case numberOfDaiTokens of
                        Just daiTokenCount ->
                            ((TokenValue.toFloatWithWarning <| daiTokenCount)
                                * daiInEth
                                * ethInUsd
                            )
                                |> TokenValue.fromFloatWithWarning
                                |> Just

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


fetchStakingInfoOrApyCmd : Chain -> Time.Posix -> Wallet -> Cmd Msg
fetchStakingInfoOrApyCmd chain now wallet =
    case userInfo wallet of
        Just uInfo ->
            fetchUserStakingInfoCmd chain uInfo.address

        Nothing ->
            fetchApyCmd chain


fetchUserStakingInfoCmd : Chain -> Address -> Cmd Msg
fetchUserStakingInfoCmd chain userAddress =
    StakingContract.getUserStakingInfo
        chain
        userAddress
        Types.StakingInfoFetched


fetchApyCmd : Chain -> Cmd Msg
fetchApyCmd chain =
    StakingContract.getApy
        chain
        Types.ApyFetched


doApproveChainCmdFarm : Chain -> UserTx.Initiator Msg
doApproveChainCmdFarm chain =
    { notifiers =
        { onMine = Just <| always Types.RefetchStakingInfoOrApy
        , onSign = Nothing
        }
    , send = StakingContract.approveLiquidityToken chain
    , txInfo = UserTx.StakingApprove
    }


doDepositChainCmdFarm : Chain -> TokenValue -> UserTx.Initiator Msg
doDepositChainCmdFarm chain amount =
    { notifiers =
        { onMine = Just <| always Types.RefetchStakingInfoOrApy
        , onSign = Just <| Types.DepositOrWithdrawSigned Types.Deposit amount
        }
    , send = StakingContract.stake chain amount
    , txInfo = UserTx.StakingDeposit amount
    }


doWithdrawChainCmdFarm : Chain -> TokenValue -> UserTx.Initiator Msg
doWithdrawChainCmdFarm chain amount =
    { notifiers =
        { onMine = Just <| always Types.RefetchStakingInfoOrApy
        , onSign = Just <| Types.DepositOrWithdrawSigned Types.Withdraw amount
        }
    , send = StakingContract.withdraw chain amount
    , txInfo = UserTx.StakingWithdraw amount
    }


doExitChainCmdFarm : Chain -> UserTx.Initiator Msg
doExitChainCmdFarm chain =
    { notifiers =
        { onMine = Just <| always Types.RefetchStakingInfoOrApy
        , onSign = Nothing
        }
    , send = StakingContract.exit chain
    , txInfo = UserTx.StakingExit
    }


doClaimRewardsFarm : Chain -> UserTx.Initiator Msg
doClaimRewardsFarm chain =
    { notifiers =
        { onMine = Just <| always Types.RefetchStakingInfoOrApy
        , onSign = Nothing
        }
    , send = StakingContract.claimRewards chain
    , txInfo = UserTx.StakingClaim
    }


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


locationCheckResultToJurisdictionStatus : Result Error (Result String LocationInfo) -> JurisdictionCheckStatus
locationCheckResultToJurisdictionStatus decodeResult =
    decodeResult
        |> Result.map
            (\checkResult ->
                checkResult
                    |> Result.map
                        (\locationInfo ->
                            Types.Checked <|
                                countryCodeToJurisdiction locationInfo.ipCode locationInfo.geoCode
                        )
                    |> Result.mapError
                        (\e ->
                            Types.Error <|
                                "Location check failed: "
                                    ++ e
                        )
                    |> Result.Extra.merge
            )
        |> Result.mapError
            (\e -> Types.Error <| "Location check response decode error: " ++ Decoder.errorToString e)
        |> Result.Extra.merge


countryCodeToJurisdiction : String -> String -> Jurisdiction
countryCodeToJurisdiction ipCode geoCode =
    let
        allowedJurisdiction =
            Set.fromList [ ipCode, geoCode ]
                |> Set.intersect ConfigFile.forbiddenJurisdictionCodes
                |> Set.isEmpty
    in
    if allowedJurisdiction then
        Types.JurisdictionsWeArentIntimidatedIntoExcluding

    else
        Types.ForbiddenJurisdictions


locationCheckDecoder : Decoder (Result String LocationInfo)
locationCheckDecoder =
    Decoder.oneOf
        [ Decoder.field "errorMessage" Decoder.string
            |> Decoder.map Err
        , locationInfoDecoder
            |> Decoder.map Ok
        ]


locationInfoDecoder : Decoder.Decoder LocationInfo
locationInfoDecoder =
    Decoder.map2
        LocationInfo
        (Decoder.field "ipCountry" Decoder.string)
        (Decoder.field "geoCountry" Decoder.string)


calcAvailableRewards : UserStakingInfo -> Time.Posix -> TokenValue
calcAvailableRewards stakingInfo now =
    let
        secondsElapsed =
            TimeHelpers.sub now stakingInfo.timestamp
                |> Time.posixToMillis
                |> toFloat
                |> (\msec -> msec / 1000)

        accrued =
            TokenValue.mulFloatWithWarning stakingInfo.rewardRate secondsElapsed
    in
    TokenValue.add stakingInfo.claimableRewards accrued


validateInput : String -> TokenValue -> Maybe TokenValue
validateInput input max =
    TokenValue.fromString input
        |> Maybe.andThen
            (\val ->
                if TokenValue.compare val TokenValue.zero == LT then
                    Nothing

                else if TokenValue.compare val max == GT then
                    Nothing

                else
                    Just val
            )


calcTimeLeft :
    Time.Posix
    -> Int
calcTimeLeft now =
    let
        timeLeft =
            Time.posixToMillis (TimeHelpers.sub (TimeHelpers.secondsToPosix ConfigFile.farmingPeriodEnds) now)
    in
    if timeLeft <= 0 then
        0

    else
        timeLeft


fetchAllPollsCmd : Cmd Msg
fetchAllPollsCmd =
    Http.request
        { method = "GET"
        , headers = []
        , url = "https://personal-rxyx.outsystemscloud.com/QuantumObserver/rest/VotingResults/GetPolls?FromPollId=0&Count=0"
        , body = Http.emptyBody
        , expect =
            Http.expectJson
                Types.PollsFetched
                pollListDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


pollListDecoder : Decoder (List Poll)
pollListDecoder =
    Decoder.list pollDecoder
        |> Decoder.map (List.sortBy .id)


pollDecoder : Decoder Poll
pollDecoder =
    Decoder.map4 Poll
        (Decoder.field "Id" Decoder.int)
        (Decoder.field "Title" Decoder.string)
        (Decoder.field "Question" Decoder.string)
        (Decoder.oneOf
            [ pollOptionListDecoder
            , Decoder.succeed []
            ]
        )


pollOptionListDecoder : Decoder (List PollOption)
pollOptionListDecoder =
    Decoder.field "Options" (Decoder.list pollOptionDecoder)
        |> Decoder.map (List.sortBy .id)


pollOptionDecoder : Decoder PollOption
pollOptionDecoder =
    Decoder.map3 PollOption
        (Decoder.field "Id" Decoder.int)
        (Decoder.field "PollId" Decoder.int)
        (Decoder.field "Name" Decoder.string)


signResponseCmd : UserInfo -> Poll -> Maybe Int -> Cmd Msg
signResponseCmd uInfo poll maybePollOptionId =
    Ports.web3Sign <|
        Json.Encode.object
            [ ( "data"
              , Json.Encode.string <|
                    encodeSignableResponse
                        poll
                        maybePollOptionId
              )
            , ( "address", Json.Encode.string (uInfo.address |> Eth.Utils.addressToChecksumString) )
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


signedResponseFromJSDecoder : Decoder.Decoder SignedResponse
signedResponseFromJSDecoder =
    Decoder.map4 SignedResponse
        (Decoder.field "address" EthHelpers.addressDecoder)
        (Decoder.field "pollId" Decoder.int)
        (Decoder.field "pollOptionId" (Decoder.nullable Decoder.int))
        (Decoder.field "sig" Decoder.string)


validateSigResultDecoder : Decoder.Decoder ( Int, SigValidationResult )
validateSigResultDecoder =
    Decoder.map2 Tuple.pair
        (Decoder.field "id" Decoder.int)
        (Decoder.field "success" Decoder.bool
            |> Decoder.map
                (\successBool ->
                    if successBool then
                        Types.Valid

                    else
                        Types.Invalid
                )
        )


fetchFryBalancesCmd : Chain -> List Address -> Cmd Msg
fetchFryBalancesCmd chain addresses =
    Contracts.FryBalanceFetch.fetch
        chain
        addresses
        Types.FryBalancesFetched


validateSignedResponsesCmd : List ResponseToValidate -> Cmd Msg
validateSignedResponsesCmd loggedSignedResponses =
    loggedSignedResponses
        |> List.map encodeResponseToValidate
        |> List.map Ports.web3ValidateSig
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
        , expect = Http.expectWhatever (Types.ResponseSent signedResponse.pollId)
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
                Types.SignedResponsesFetched
                signedResponsesDictFromServerDecoder
        }


signedResponsesDictFromServerDecoder : Decoder.Decoder (Dict Int SignedResponse)
signedResponsesDictFromServerDecoder =
    Decoder.list Decoder.value
        |> Decoder.map (List.map (Decoder.decodeValue loggedSignedResponseFromServerDecoder))
        |> Decoder.map (List.filterMap Result.toMaybe)
        |> Decoder.map Dict.fromList


loggedSignedResponseFromServerDecoder : Decoder.Decoder ( Int, SignedResponse )
loggedSignedResponseFromServerDecoder =
    Decoder.field "Vote"
        (Decoder.map2 Tuple.pair
            (Decoder.field "Id" Decoder.int)
            (Decoder.map4 SignedResponse
                (Decoder.field "Address" <| EthHelpers.addressDecoder)
                (Decoder.field "PollId" <| Decoder.int)
                (Decoder.maybe <| Decoder.field "OptionId" Decoder.int)
                (Decoder.field "Signature" <| Decoder.string)
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


fetchEthBalance : Chain -> Wallet -> Cmd Msg
fetchEthBalance chain wallet =
    case userInfo wallet of
        Nothing ->
            Cmd.none

        Just uInfo ->
            ERC20.getEthBalance
                chain
                uInfo.address
                Types.UserEthBalanceFetched


fetchDerivedEthBalance : Chain -> Wallet -> Cmd Msg
fetchDerivedEthBalance chain wallet =
    case userInfo wallet of
        Nothing ->
            Cmd.none

        Just uInfo ->
            ERC20.getBalanceCmd
                chain
                ConfigFile.derivedEthContractAddress
                uInfo.address
                Types.UserDerivedEthBalanceFetched


fetchIssuanceDetail : Chain -> String -> Cmd Msg
fetchIssuanceDetail chain depositAmount =
    case TokenValue.fromString depositAmount of
        Nothing ->
            Cmd.none

        Just amount ->
            Death.getIssuanceDetail
                amount
                Types.DerivedEthIssuanceDetailFetched
                chain


fetchDethPositionInfo : Chain -> Maybe TokenValue -> Cmd Msg
fetchDethPositionInfo chain amount =
    case amount of
        Nothing ->
            Cmd.none

        Just dEthVal ->
            Death.getRedeemable
                dEthVal
                Types.DerivedEthRedeemableFetched
                chain


doDepositChainCmd : Address -> TokenValue -> UserTx.Initiator Msg
doDepositChainCmd sender amount =
    { notifiers =
        { onMine =
            always
                Types.FetchUserEthBalance
                |> Just
        , onSign =
            Types.DepositSigned
                |> Just
        }
    , send = Death.deposit sender amount
    , txInfo = UserTx.DEthDeposit
    }


doWithdrawChainCmd : Address -> TokenValue -> UserTx.Initiator Msg
doWithdrawChainCmd receiver amount =
    { notifiers =
        { onMine =
            always
                Types.FetchUserDerivedEthBalance
                |> Just
        , onSign =
            Types.WithdrawSigned
                |> Just
        }
    , send = Death.redeem receiver amount
    , txInfo = UserTx.DEthRedeem
    }


derivedEthInfoInit : UserDerivedEthInfo
derivedEthInfoInit =
    { ethBalance = TokenValue.zero
    , dEthBalance = TokenValue.zero
    , totalCollateralRedeemed = TokenValue.zero
    , redeemFee = TokenValue.zero
    , collateralReturned = TokenValue.zero
    , dEthAllowance = TokenValue.zero
    , actualCollateralAdded = TokenValue.zero
    , depositFee = TokenValue.zero
    , tokensIssued = TokenValue.zero
    }


userInfo : Wallet -> Maybe UserInfo
userInfo walletState =
    case walletState of
        Types.Active uInfo ->
            Just uInfo

        _ ->
            Nothing


initiateUserTxs : TxSentry.TxSentry Msg -> UserTx.Tracker Msg -> List (UserTx.Initiator Msg) -> ( TxSentry.TxSentry Msg, Cmd Msg, UserTx.Tracker Msg )
initiateUserTxs txSentry prevTrackedTxs txInitiators =
    List.foldl
        (\initiator ( accTxSentry, accCmd, accTrackedTxs ) ->
            initiateUserTx accTxSentry accTrackedTxs initiator
                |> tuple3MapSecond
                    (\newCmd ->
                        Cmd.batch
                            [ accCmd
                            , newCmd
                            ]
                    )
        )
        ( txSentry, Cmd.none, prevTrackedTxs )
        txInitiators


initiateUserTx : TxSentry.TxSentry Msg -> UserTx.Tracker Msg -> UserTx.Initiator Msg -> ( TxSentry.TxSentry Msg, Cmd Msg, UserTx.Tracker Msg )
initiateUserTx txSentry prevTrackedTxs txInitiator =
    let
        ( trackedTxId, newTrackedTxs ) =
            prevTrackedTxs
                |> addTrackedTx txInitiator.txInfo txInitiator.notifiers

        ( newTxSentry, txSentryCmd ) =
            UserTx.execute
                txSentry
                { txInitiator
                    | notifiers = trackingNotifiers trackedTxId
                }
    in
    ( newTxSentry
    , txSentryCmd
    , newTrackedTxs
    )


trackingNotifiers : Int -> UserTx.Notifiers Msg
trackingNotifiers trackedTxId =
    { onSign = Just <| Types.TxSigned trackedTxId
    , onMine = Just <| Types.TxMined trackedTxId
    }


addTrackedTx : TxInfo -> UserTx.Notifiers Msg -> UserTx.Tracker Msg -> ( Int, UserTx.Tracker Msg )
addTrackedTx userTx notifiers tracker =
    let
        newTrackedTx =
            UserTx.TrackedTx
                notifiers
                userTx
                UserTx.Signing
    in
    ( List.length tracker
    , List.append
        tracker
        [ newTrackedTx ]
    )


emptyConfig : Config
emptyConfig =
    { xDai =
        { chain = Types.Eth
        , contract = emptyAddress
        , startScanBlock = 0
        , providerUrl = ""
        }
    , ethereum =
        { chain = Types.Eth
        , contract = emptyAddress
        , startScanBlock = 0
        , providerUrl = ""
        }
    , bsc =
        { chain = Types.BSC
        , contract = emptyAddress
        , startScanBlock = 0
        , providerUrl = ""
        }
    }


emptyAddress : Address
emptyAddress =
    Eth.Utils.unsafeToAddress ""
