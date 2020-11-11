module Sentiment.View exposing (view)

import AddressDict exposing (AddressDict)
import Common.Msg exposing (..)
import Common.Types exposing (..)
import Common.View exposing (..)
import Config
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import ElementMarkdown
import Eth.Types exposing (Address)
import Eth.Utils
import FormatFloat
import Helpers.Element as EH exposing (DisplayProfile(..), changeForMobile, responsiveVal)
import Helpers.Tuple as TupleHelpers
import Html.Events
import Images
import Json.Encode exposing (float)
import List.Extra
import Maybe.Extra
import Phace
import Result.Extra
import Routing exposing (Route)
import Sentiment.Types exposing (..)
import Theme exposing (darkTheme, defaultTheme)
import TokenValue exposing (TokenValue)
import Wallet exposing (Wallet)


view : DisplayProfile -> Maybe UserInfo -> Model -> Element Msg
view dProfile maybeUserInfo model =
    Element.el
        [ responsiveVal dProfile
            (Element.paddingXY 100 50)
            (Element.paddingXY 20 20)
        , Element.width Element.fill
        , Element.Font.color EH.white
        ]
    <|
        case model.polls of
            Nothing ->
                titleText dProfile "Loading polls..."

            Just polls ->
                Element.column
                    [ Element.width Element.fill
                    , Element.spacing 50
                    ]
                    [ titleText dProfile "Foundry Polls"
                    , viewPolls dProfile maybeUserInfo polls model.validatedResponses model.fryBalances model.mouseoverState
                    ]


titleText : DisplayProfile -> String -> Element Msg
titleText dProfile title =
    Element.el
        [ Element.Font.bold
        , Element.Font.size <| responsiveVal dProfile 50 18
        ]
    <|
        Element.text title


viewPolls : DisplayProfile -> Maybe UserInfo -> List Poll -> ValidatedResponseTracker -> AddressDict (Maybe TokenValue) -> MouseoverState -> Element Msg
viewPolls dProfile maybeUserInfo polls validatedResponses fryBalances mouseoverState =
    Element.column
        [ Element.spacing 20 ]
        (List.map
            (viewPoll dProfile maybeUserInfo validatedResponses fryBalances mouseoverState)
            (List.reverse polls)
        )


viewPoll : DisplayProfile -> Maybe UserInfo -> ValidatedResponseTracker -> AddressDict (Maybe TokenValue) -> MouseoverState -> Poll -> Element Msg
viewPoll dProfile maybeUserInfo validatedResponses fryBalances mouseoverState poll =
    let
        validatedResponsesForPoll =
            validatedResponses
                |> Dict.get poll.id
                |> Maybe.withDefault AddressDict.empty

        foldFunc : ( Address, ValidatedResponse ) -> Dict Int ( TokenValue, AddressDict TokenValue ) -> Dict Int ( TokenValue, AddressDict TokenValue )
        foldFunc ( address, validatedResponse ) accTallyData =
            case validatedResponse.maybePollOptionId of
                Nothing ->
                    accTallyData

                Just pollOptionId ->
                    let
                        fryAmount =
                            fryBalances
                                |> AddressDict.get address
                                |> Maybe.Extra.join
                                |> Maybe.withDefault TokenValue.zero
                    in
                    accTallyData
                        |> Dict.update pollOptionId
                            (Maybe.withDefault ( TokenValue.zero, AddressDict.empty )
                                >> (\( accTotal, accTallies ) ->
                                        Just
                                            ( TokenValue.add accTotal fryAmount
                                            , accTallies
                                                |> AddressDict.update address
                                                    (Maybe.withDefault TokenValue.zero
                                                        >> TokenValue.add fryAmount
                                                        >> Just
                                                    )
                                            )
                                   )
                            )

        talliedFryForOptions =
            validatedResponsesForPoll
                |> AddressDict.toList
                |> List.foldl
                    foldFunc
                    Dict.empty

        totalFryVoted =
            talliedFryForOptions
                |> Dict.foldl
                    (\_ ( fryForOption, _ ) ->
                        TokenValue.add fryForOption
                    )
                    TokenValue.zero
    in
    Element.column
        [ Element.spacing 10
        , Element.padding 10
        , Element.Background.color <| Element.rgba 1 1 1 0.1
        , Element.Border.rounded 10
        ]
        [ Element.el
            [ Element.Font.size <| responsiveVal dProfile 26 12 ]
            (ElementMarkdown.renderString [] poll.question
                |> Result.Extra.extract
                    (\errStr ->
                        Element.el
                            [ Element.Font.color Theme.softRed ]
                        <|
                            Element.text ("markdown render error: " ++ errStr)
                    )
            )
        , Element.el
            [ Element.padding 10
            , Element.width Element.fill
            ]
            (viewOptions dProfile maybeUserInfo poll talliedFryForOptions totalFryVoted mouseoverState)
        ]


viewOptions : DisplayProfile -> Maybe UserInfo -> Poll -> Dict Int ( TokenValue, AddressDict TokenValue ) -> TokenValue -> MouseoverState -> Element Msg
viewOptions dProfile maybeUserInfo poll talliedVotes totalFryVoted mouseoverState =
    Element.column
        [ Element.spacing 10
        , Element.width <| Element.px (changeForMobile 200 dProfile 1000)
        ]
        (poll.options
            |> List.map
                (\option ->
                    let
                        talliedForOption =
                            talliedVotes
                                |> Dict.get option.id
                                |> Maybe.withDefault ( TokenValue.zero, AddressDict.empty )

                        supportFloat =
                            if totalFryVoted |> TokenValue.isZero then
                                0

                            else
                                TokenValue.getRatioWithWarning
                                    (Tuple.first talliedForOption)
                                    totalFryVoted
                    in
                    viewOption dProfile
                        maybeUserInfo
                        poll
                        option
                        ( totalFryVoted, supportFloat )
                        talliedForOption
                        mouseoverState
                )
        )


viewOption : DisplayProfile -> Maybe UserInfo -> Poll -> PollOption -> ( TokenValue, Float ) -> ( TokenValue, AddressDict TokenValue ) -> MouseoverState -> Element Msg
viewOption dProfile maybeUserInfo poll pollOption ( totalVotes, supportFloat ) ( totalVotesInSupport, detailedSupportDict ) mouseoverState =
    let
        wholeOptionClick =
            case maybeUserInfo of
                Nothing ->
                    Element.Events.onClick <|
                        OptionClicked
                            Nothing
                            poll
                            Nothing

                Just userInfo ->
                    let
                        userSupportsOption =
                            AddressDict.member
                                userInfo.address
                                detailedSupportDict
                    in
                    if userSupportsOption then
                        Element.Events.onClick <|
                            OptionClicked
                                (Just
                                    userInfo
                                )
                                poll
                                Nothing

                    else
                        Element.Events.onClick <|
                            OptionClicked
                                (Just userInfo)
                                poll
                                (Just pollOption.id)

        voteButtonElementOrNone =
            case maybeUserInfo of
                Nothing ->
                    Element.none

                Just userInfo ->
                    let
                        userSupportsOption =
                            AddressDict.member
                                userInfo.address
                                detailedSupportDict
                    in
                    if userSupportsOption then
                        Images.toElement
                            [ Element.alignRight
                            , Element.height <| Element.px (changeForMobile 20 dProfile 40)
                            , Element.width <| Element.px (changeForMobile 20 dProfile 40)
                            , Element.pointer

                            -- , Element.Events.onClick <|
                            --     OptionClicked
                            --         (Just userInfo)
                            --         poll
                            --         Nothing
                            ]
                            Images.fryIcon

                    else
                        Images.toElement
                            [ Element.alignRight
                            , Element.height <| Element.px (changeForMobile 20 dProfile 40)
                            , Element.width <| Element.px (changeForMobile 20 dProfile 40)
                            , Element.pointer

                            -- , Element.Events.onClick <|
                            --     OptionClicked
                            --         (Just userInfo)
                            --         poll
                            --         (Just pollOption.id)
                            , Element.mouseOver
                                [ Element.alpha 1 ]
                            , Element.inFront <|
                                Images.toElement
                                    [ Element.width Element.fill
                                    , Element.height Element.fill
                                    , Element.alpha 0
                                    , Element.mouseOver
                                        [ Element.alpha 1 ]
                                    ]
                                    Images.pollChoiceMouseover
                            ]
                            Images.pollChoiceEmpty
    in
    Element.row
        [ Element.width Element.fill
        , Element.spacing 10
        ]
        [ Element.row
            [ Element.width Element.fill
            , Element.spacing 15
            , Element.pointer
            , wholeOptionClick
            ]
          <|
            [ voteButtonElementOrNone
            , Element.paragraph
                [ Element.Font.size <| responsiveVal dProfile 18 10
                , Element.alignLeft
                ]
                [ Element.text pollOption.name ]
            ]
        , Element.row
            [ Element.width Element.fill
            , Element.spacing 5
            ]
            [ Element.el
                [ Element.alignLeft
                , Element.width <| Element.px <| maxBarWidth + 5
                ]
              <|
                voteBarBreakdown dProfile maybeUserInfo ( poll.id, pollOption.id ) totalVotes totalVotesInSupport detailedSupportDict mouseoverState
            , Element.el
                [ Element.width <| Element.px 50 ]
                (Element.text <|
                    (FormatFloat.formatFloat 1 (supportFloat * 100)
                        ++ "%"
                    )
                )
            , viewFryAmount totalVotesInSupport
            ]
        ]


maxBarWidth : Int
maxBarWidth =
    200


type alias VoteBarBlock =
    { x : Int
    , width : Int
    , colorRgb : ( Float, Float, Float )
    , address : Address
    , amount : TokenValue
    }


voteBarBreakdown : DisplayProfile -> Maybe UserInfo -> ( Int, Int ) -> TokenValue -> TokenValue -> AddressDict TokenValue -> MouseoverState -> Element Msg
voteBarBreakdown dProfile maybeUserInfo ( pollId, pollOptionId ) totalVotes totalVotesInSupport detailedSupportDict mouseoverState =
    let
        votesToBarWidth tokens =
            let
                ratio =
                    if TokenValue.isZero totalVotes then
                        0

                    else
                        TokenValue.getRatioWithWarning tokens totalVotes
            in
            ratio
                * (maxBarWidth |> toFloat)
                |> floor

        fullBarWidth =
            votesToBarWidth totalVotesInSupport + 2

        orderedVotes =
            detailedSupportDict
                |> AddressDict.toList
                |> List.sortBy (Tuple.second >> TokenValue.toFloatWithWarning >> negate)
                |> List.reverse

        folderFunc : ( Address, TokenValue ) -> List VoteBarBlock -> List VoteBarBlock
        folderFunc ( voter, tokens ) accBlocks =
            let
                width =
                    votesToBarWidth tokens

                x =
                    List.Extra.last accBlocks
                        |> Maybe.map
                            (\block -> block.x + block.width)
                        |> Maybe.withDefault 0

                maybeColorAttribute =
                    Phace.faceColorFromAddress voter
                        |> Result.toMaybe

                newBlock =
                    { width = width
                    , x = x
                    , colorRgb =
                        maybeColorAttribute
                            |> Maybe.withDefault ( 0, 0, 0 )
                    , address = voter
                    , amount = tokens
                    }
            in
            accBlocks
                |> List.append
                    [ newBlock ]

        blocks =
            orderedVotes
                |> List.foldl
                    folderFunc
                    []
    in
    Element.row
        [ Element.height <| Element.px 30
        , Element.width <| Element.px fullBarWidth
        , Element.Background.color Theme.lightGray
        , Element.Border.glow
            (Element.rgba 1 1 1 0.2)
            3
        , Element.Border.width 1
        , Element.Border.color Theme.lightBlue
        , Element.htmlAttribute <|
            Html.Events.onMouseLeave <|
                SetMouseoverState None
        ]
        (blocks
            |> List.indexedMap
                (\blockId block ->
                    if block.width > 0 then
                        let
                            thisVoterBlockInfo =
                                { pollId = pollId
                                , pollOptionId = pollOptionId
                                , blockId = blockId
                                }
                        in
                        Element.el
                            ([ Element.height Element.fill
                             , Element.width <| Element.px block.width
                             , Element.htmlAttribute <|
                                Html.Events.onMouseEnter <|
                                    SetMouseoverState <|
                                        VoterBlock
                                            thisVoterBlockInfo
                             , Element.Background.color <|
                                Element.rgb
                                    (TupleHelpers.tuple3First block.colorRgb)
                                    (TupleHelpers.tuple3Second block.colorRgb)
                                    (TupleHelpers.tuple3Third block.colorRgb)
                             ]
                                ++ (case mouseoverState of
                                        VoterBlock mouseoverVoterBlockInfo ->
                                            if thisVoterBlockInfo == mouseoverVoterBlockInfo then
                                                [ Element.above <|
                                                    Element.column
                                                        [ Element.alignLeft ]
                                                        [ Element.el
                                                            [ Element.Border.glow Theme.lightBlue 4
                                                            , Element.Border.rounded 10
                                                            , Element.clip
                                                            , Element.Border.width 2
                                                            , Element.Border.color EH.black
                                                            ]
                                                          <|
                                                            Element.html <|
                                                                Phace.fromEthAddress block.address
                                                        , Element.el
                                                            [ Element.Font.size 14
                                                            , Element.Font.color EH.black
                                                            , Element.Background.color Theme.lightBlue
                                                            , Element.Border.width 1
                                                            , Element.Border.color EH.black
                                                            , Element.padding 2
                                                            ]
                                                          <|
                                                            Element.text <|
                                                                Eth.Utils.addressToChecksumString block.address
                                                        , Element.el [ Element.width Element.fill, Element.height <| Element.px 2 ] Element.none
                                                        ]
                                                ]

                                            else
                                                []

                                        _ ->
                                            []
                                   )
                            )
                            Element.none

                    else
                        Element.none
                )
        )


viewFryAmount : TokenValue -> Element Msg
viewFryAmount amount =
    Element.row
        [ Element.moveRight 20
        , Element.spacing 8
        , Element.centerY
        ]
        [ Images.toElement
            [ Element.alignTop
            ]
            Images.fryIcon
        , Element.text <|
            TokenValue.toConciseString amount
        ]
