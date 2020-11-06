module Farm.State exposing (..)

import ChainCmd exposing (ChainCmd)
import Common.Msg exposing (MsgDown, MsgUp)
import Common.Types exposing (..)
import Contracts.Staking as StakingContract
import Eth.Types exposing (Address)
import Farm.Types exposing (..)
import Time
import TokenValue exposing (TokenValue)
import UserNotice as UN
import Wallet exposing (Wallet)


init : Maybe UserInfo -> Time.Posix -> ( Model, Cmd Msg )
init maybeUserInfo now =
    ( { userStakingInfo = Nothing
      , depositWithdrawUXModel = Nothing
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
                ChainCmd.none
                [ msgUp ]

        UpdateNow newNow ->
            justModelUpdate
                { prevModel | now = newNow }

        AmountInputChanged newInput ->
            case prevModel.depositWithdrawUXModel of
                Just ( depositOrWithdraw, amountInputUXModel ) ->
                    justModelUpdate
                        { prevModel
                            | depositWithdrawUXModel =
                                Just
                                    ( depositOrWithdraw
                                    , { amountInputUXModel
                                        | amountInput = newInput
                                      }
                                    )
                        }

                Nothing ->
                    justModelUpdate prevModel

        DoUnlock ->
            UpdateResult
                prevModel
                Cmd.none
                doApproveChainCmd
                []

        StartDeposit ->
            justModelUpdate
                { prevModel
                    | depositWithdrawUXModel = Just ( Deposit, { amountInput = "" } )
                }

        StartWithdraw ->
            justModelUpdate
                { prevModel
                    | depositWithdrawUXModel = Just ( Withdraw, { amountInput = "" } )
                }

        DoExit ->
            Debug.todo ""

        DoDeposit amount ->
            UpdateResult
                prevModel
                Cmd.none
                (doDepositChainCmd amount)
                []

        DoWithdraw amount ->
            Debug.todo ""

        StakingInfoFetched fetchResult ->
            case fetchResult of
                Err httpErr ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ Common.Msg.AddUserNotice <| UN.web3FetchError "staking info" httpErr ]

                Ok userStakingInfo ->
                    justModelUpdate
                        { prevModel
                            | userStakingInfo =
                                Just userStakingInfo
                        }

        FakeFetchBalanceInfo ->
            justModelUpdate
                { prevModel
                    | userStakingInfo =
                        Just <|
                            { unstaked = TokenValue.fromIntTokenValue 10
                            , staked = TokenValue.fromIntTokenValue 10
                            , claimableRewards = TokenValue.zero
                            , rewardRate = TokenValue.fromIntTokenValue 1
                            , timestamp = prevModel.now
                            }
                }


runMsgDown : MsgDown -> Model -> UpdateResult
runMsgDown msg prevModel =
    case msg of
        Common.Msg.UpdateWallet newWallet ->
            let
                newModel =
                    { prevModel | userStakingInfo = Nothing }

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
                ChainCmd.none
                []


fetchUserStakingInfoCmd : Address -> Cmd Msg
fetchUserStakingInfoCmd userAddress =
    StakingContract.getUserStakingInfo
        userAddress
        StakingInfoFetched


doDepositChainCmd : TokenValue -> ChainCmd Msg
doDepositChainCmd amount =
    ChainCmd.custom
        { onMined = Nothing
        , onSign = Nothing
        , onBroadcast = Nothing
        }
        (StakingContract.stake amount)


doApproveChainCmd : ChainCmd Msg
doApproveChainCmd =
    ChainCmd.custom
        { onMined = Nothing
        , onSign = Nothing
        , onBroadcast = Nothing
        }
        StakingContract.approveLiquidityToken



-- doDepositCmd : TokenValue -> Cmd Msg
-- doDepositCmd amount =
--     StakingContract.callStake
--         amount
--         (always NoOp)


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 100 UpdateNow
