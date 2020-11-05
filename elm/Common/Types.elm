module Common.Types exposing (..)

import Time
import Dict exposing (Dict)
import Eth.Net
import Eth.Sentry.Tx as TxSentry
import Eth.Types exposing (Address, Hex, TxHash)
import Json.Decode
import Json.Encode
import TokenValue exposing (TokenValue)


type alias UserInfo =
    { network : Eth.Net.NetworkId
    , address : Address
    -- , balance : Maybe TokenValue
    -- , unlockStatus : UnlockStatus
    }


-- type UnlockStatus
--     = NotConnected
--     | Checking
--     | Locked
--     | Unlocking
--     | Unlocked


-- withBalance : TokenValue -> UserInfo -> UserInfo
-- withBalance balance userInfo =
--     { userInfo
--         | balance = Just balance
--     }


-- withUnlockStatus : UnlockStatus -> UserInfo -> UserInfo
-- withUnlockStatus unlockStatus userInfo =
--     { userInfo
--         | unlockStatus = unlockStatus
--     }

type PhaceIconId
    = UserPhace


-- type alias TrackedTx =
--     { txHash : TxHash
--     , txInfo : TxInfo
--     , status : TxStatus
--     }


-- type TxInfo
--     = None -- unused for now


-- type TxStatus
--     = Mining
--     | Failed FailReason
--     | Mined


-- type FailReason
--     = MinedButExecutionFailed


type alias UserStakingInfo =
    { unstaked : TokenValue
    , staked : TokenValue
    , claimableRewards : TokenValue
    , rewardRate : TokenValue
    , timestamp : Time.Posix
    }