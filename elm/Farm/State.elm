module Farm.State exposing (..)

import Common.Msg exposing (MsgDown, MsgUp)
import Common.Types exposing (..)
import Eth.Types exposing (Address)
import Farm.Types exposing (..)
import Time
import TokenValue exposing (TokenValue)
import Wallet exposing (Wallet)


init : Maybe UserInfo -> Time.Posix -> ( Model, Cmd Msg )
init maybeUserInfo now =
    ( { timedUserStakingInfo = Nothing
      , depositWithdrawUXModel =
            { inMenu = Nothing }
      , now = now
      }
    , case maybeUserInfo of
        Just userInfo ->
            fetchUserStakingInfoCmd userInfo.address

        Nothing ->
            Cmd.none
    )


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        NoOp ->
            justModelUpdate prevModel

        MsgUp msgUp ->
            UpdateResult
                prevModel
                Cmd.none
                [ msgUp ]

        UpdateNow newNow ->
            justModelUpdate
                { prevModel | now = newNow }

        StartDeposit ->
            Debug.todo ""

        StartWithdraw ->
            Debug.todo ""

        FakeFetchBalanceInfo ->
            justModelUpdate
                { prevModel
                    | timedUserStakingInfo =
                        Just <|
                            { userStakingInfo =
                                { unstaked = TokenValue.fromIntTokenValue 10
                                , staked = TokenValue.fromIntTokenValue 10
                                , claimableRewards = TokenValue.zero
                                }
                            , time = prevModel.now
                            }
                }


runMsgDown : MsgDown -> Model -> UpdateResult
runMsgDown msg prevModel =
    case msg of
        Common.Msg.UpdateWallet newWallet ->
            let
                newModel =
                    { prevModel | timedUserStakingInfo = Nothing }

                cmd =
                    case Wallet.userInfo newWallet of
                        Just userInfo ->
                            fetchUserStakingInfoCmd userInfo.address

                        Nothing ->
                            Cmd.none
            in
            UpdateResult
                newModel
                cmd
                []


fetchUserStakingInfoCmd : Address -> Cmd Msg
fetchUserStakingInfoCmd userAddress =
    Cmd.none


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 100 UpdateNow
