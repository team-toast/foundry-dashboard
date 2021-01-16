module Common.Types exposing (..)

import Contracts.UniSwapGraph.Object exposing (Token)
import Dict exposing (Dict)
import Eth.Net
import Eth.Sentry.Tx as TxSentry
import Eth.Types exposing (Address, Hex, TxHash)
import Json.Decode
import Json.Encode
import Time
import TokenValue exposing (TokenValue)


type alias UserInfo =
    { network : Eth.Net.NetworkId
    , address : Address
    }


type alias GTagData =
    { event : String
    , category : String
    , label : String
    , value : Int
    }


type PhaceIconId
    = UserPhace


type alias UserStakingInfo =
    { unstaked : TokenValue
    , allowance : TokenValue
    , staked : TokenValue
    , claimableRewards : TokenValue
    , rewardRate : TokenValue
    , timestamp : Time.Posix
    }


type alias UserDerivedEthInfo =
    { ethBalance : TokenValue
    , dEthBalance : TokenValue
    , totalCollateralRedeemed : TokenValue
    , fee : TokenValue
    , collateralReturned : TokenValue
    }


type alias DepositOrWithdrawUXModel =
    Maybe ( DepositOrWithdraw, AmountUXModel )


type alias AmountUXModel =
    { amountInput : String
    }


type DepositOrWithdraw
    = Deposit
    | Withdraw


type Jurisdiction
    = ForbiddenJurisdictions
    | JurisdictionsWeArentIntimidatedIntoExcluding


type JurisdictionCheckStatus
    = WaitingForClick
    | Checking
    | Checked Jurisdiction
    | Error String


type alias LocationInfo =
    { ipCode : String
    , geoCode : String
    }
