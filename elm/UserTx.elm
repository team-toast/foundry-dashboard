module UserTx exposing (..)

import Eth.Sentry.Tx as TxSentry exposing (TxSentry)
import Eth.Types as Eth
import List.Extra
import TokenValue exposing (TokenValue)


type alias Initiator msg =
    { notifiers : Notifiers msg
    , send : Eth.Send
    , txInfo : TxInfo
    }


type alias Notifiers msg =
    { onSign : Maybe (Result String Eth.TxHash -> msg)
    , onMine : Maybe (Result String Eth.TxReceipt -> msg)
    , onBroadcast : Maybe (Result String Eth.Tx -> msg)
    }


type TxInfo
    = StakingApprove
    | StakingDeposit TokenValue
    | StakingWithdraw TokenValue
    | StakingClaim
    | StakingExit


type alias Tracker msg =
    List (TrackedTx msg)


type alias TrackedTx msg =
    { notifiers : Notifiers msg
    , txInfo : TxInfo
    , status : TxStatus
    }


type TxStatus
    = Signing
    | Rejected
    | Signed Eth.TxHash SignedTxStatus


type SignedTxStatus
    = Mining
    | Success Eth.TxReceipt
    | Failed


mapInitiator : (subMsg -> msg) -> Initiator subMsg -> Initiator msg
mapInitiator mapper initiator =
    { notifiers =
        { onSign = Maybe.map ((<<) mapper) initiator.notifiers.onSign
        , onMine = Maybe.map ((<<) mapper) initiator.notifiers.onMine
        , onBroadcast = Maybe.map ((<<) mapper) initiator.notifiers.onBroadcast
        }
    , send = initiator.send
    , txInfo = initiator.txInfo
    }


mapInitiatorList : (subMsg -> msg) -> List (Initiator subMsg) -> List (Initiator msg)
mapInitiatorList mapper =
    List.map (mapInitiator mapper)


execute : TxSentry msg -> Initiator msg -> ( TxSentry msg, Cmd msg )
execute txSentry initiator =
    TxSentry.customSend
        txSentry
        (notifiersToEthCustomSend initiator.notifiers)
        initiator.send


notifiersToEthCustomSend : Notifiers msg -> TxSentry.CustomSend msg
notifiersToEthCustomSend notifiers =
    { onSign = notifiers.onSign
    , onMined = Maybe.map (\n -> Tuple.pair n Nothing) notifiers.onMine
    , onBroadcast = notifiers.onBroadcast
    }


setTrackedTxStatus : Int -> TxStatus -> Tracker msg -> Tracker msg
setTrackedTxStatus trackedTxId newStatus =
    List.Extra.updateAt trackedTxId
        (\trackedTx ->
            { trackedTx | status = newStatus }
        )


setTrackedTxSignedStatus : Int -> SignedTxStatus -> Tracker msg -> Tracker msg
setTrackedTxSignedStatus trackedTxId newSignedStatus =
    List.Extra.updateAt trackedTxId
        (\trackedTx ->
            case trackedTx.status of
                Signed txHash _ ->
                    { trackedTx
                        | status = Signed txHash newSignedStatus
                    }

                _ ->
                    trackedTx
        )
